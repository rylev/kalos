#[macro_use]
extern crate nom;
use nom::*;
use nom::IResult::*;


#[derive(PartialEq, Clone, Debug)]
pub enum Expression<'a> {
    Arithmetic(ArithmeticExpression),
    Variable(&'a str),
    FunctionCall(&'a str, Vec<Expression<'a>>)
}

#[derive(PartialEq, Clone, Debug)]
pub enum Operator {
    Addition,
    Subtraction,
    Multiplication,
    Division
}

#[derive(PartialEq, Clone, Debug)]
pub enum ArithmeticExpression {
    Value(f64),
    Add(Box<ArithmeticExpression>, Box<ArithmeticExpression>),
    Subtract(Box<ArithmeticExpression>, Box<ArithmeticExpression>),
    Multiply(Box<ArithmeticExpression>, Box<ArithmeticExpression>),
    Divide(Box<ArithmeticExpression>, Box<ArithmeticExpression>),
    Enclosed(Box<ArithmeticExpression>)
}

named!(f64<f64>, map_res!(map_res!(digit, std::str::from_utf8), |n: &str| n.parse()));
named!(float<ArithmeticExpression>,
       chain!(
           opt!(space) ~
           val: map!(f64, ArithmeticExpression::Value) ~
           opt!(space),
           || val));
named!(enclosed_expression<ArithmeticExpression>,
    delimited!(
        chain!(opt!(space) ~ c: char!('(') ~ opt!(space),|| c),
        alt!(low_precedence),
        chain!(opt!(space) ~ c: char!(')') ~ opt!(space),|| c)));
named!(lhs<ArithmeticExpression>,
       alt_complete!(float | enclosed_expression));

named!(multiplication<(Operator, ArithmeticExpression)>,
    chain!(tag!("*") ~ mul: lhs, || (Operator::Multiplication, mul)));
named!(division<(Operator, ArithmeticExpression)>,
    chain!(tag!("/") ~ div: lhs, || (Operator::Division, div)));
named!(high_precedence<ArithmeticExpression>, chain!(
        initial: lhs ~
        remainder: many0!(alt!(multiplication | division)),
        || fold_exprs(initial, remainder)));

named!(addition<(Operator, ArithmeticExpression)>,
       chain!(tag!("+") ~ add: high_precedence, || (Operator::Addition, add)));
named!(subtraction<(Operator, ArithmeticExpression)>,
       chain!(tag!("-") ~ sub: high_precedence, || (Operator::Subtraction, sub)));
named!(low_precedence<ArithmeticExpression>, chain!(
    initial: high_precedence ~
    remainder: many0!(alt!(addition | subtraction)),
    || fold_exprs(initial, remainder)));

fn fold_exprs(initial: ArithmeticExpression, remainder: Vec<(Operator, ArithmeticExpression)>) -> ArithmeticExpression {
    remainder.into_iter().fold(initial, |acc, pair| {
        match pair {
            (Operator::Addition, expr) => ArithmeticExpression::Add(Box::new(acc), Box::new(expr)),
            (Operator::Subtraction, expr) => ArithmeticExpression::Subtract(Box::new(acc), Box::new(expr)),
            (Operator::Multiplication, expr) => ArithmeticExpression::Multiply(Box::new(acc), Box::new(expr)),
            (Operator::Division, expr) => ArithmeticExpression::Divide(Box::new(acc), Box::new(expr)),
        }
    })
}

named!(arithmetic_expression<Expression>, map!(low_precedence, |exp| Expression::Arithmetic(exp)));

named!(expression<Expression>,
    complete!(
        chain!(
            exp: alt_complete!(arithmetic_expression),
            || exp)));

named!(delimiter, tag!(";"));

#[derive(PartialEq, Clone, Debug)]
pub struct Prototype<'a>  {
    name: &'a str,
    args: Vec<&'a str>
}

#[derive(PartialEq, Clone, Debug)]
pub struct Function<'a> {
    prototype: Box<Prototype<'a>>,
    body: Box<Expression<'a>>
}

#[derive(PartialEq, Debug)]
pub enum ParserError<'a> {
    NotFinished(&'a str),
    FailedParse(&'a str),
    WTF
}

pub type ParseResult<'a, T> = Result<T, ParserError<'a>>;

pub fn parse<'a, T: 'a, NomParser>(input: &'a str, parser: NomParser) -> ParseResult<'a, T> where NomParser: Fn(&'a [u8]) -> IResult<&[u8], T> {
    match  parser(input.as_bytes()) {
        Done(leftover, _) if !leftover.is_empty() => {
            // TODO: remove unwrap
            Err(ParserError::NotFinished(std::str::from_utf8(leftover).unwrap()))
        }
        Done(_, result) => Ok(result),
        Error(Err::Position(t, bytes)) => {
            println!("{:?}", t);
            Err(ParserError::FailedParse(std::str::from_utf8(bytes).unwrap()))
        },
        _ => Err(ParserError::WTF),
    }
}

#[test]
fn it_parses_binary_arithmetic() {
    let normal = "(1 / 10) + (90 * 67)";
    let condensed = "(1/10)+(90*67)";
    let weird = "(1/10 ) +(  90* 67   )";
    let lhs =
        Box::new(
            ArithmeticExpression::Divide(
                Box::new(ArithmeticExpression::Value(1.0)),
                Box::new(ArithmeticExpression::Value(10.0))));
    let rhs =
        Box::new(
            ArithmeticExpression::Multiply(
                Box::new(ArithmeticExpression::Value(90.0)),
                Box::new(ArithmeticExpression::Value(67.0))));

    let ast = Expression::Arithmetic(ArithmeticExpression::Add(lhs, rhs));
    let expected = Ok(ast);

    assert_eq!(expected, parse(normal, expression));
    assert_eq!(expected, parse(condensed, expression));
    assert_eq!(expected, parse(weird, expression));
}

#[test]
fn it_parses_chained_arithmetic() {
    let normal = "1 + 3 * 5 + 2 / 5";
    let divide =
        ArithmeticExpression::Divide(
            Box::new(ArithmeticExpression::Value(2.0)),
            Box::new(ArithmeticExpression::Value(5.0)));
    let multiply =
        ArithmeticExpression::Multiply(
            Box::new(ArithmeticExpression::Value(3.0)),
            Box::new(ArithmeticExpression::Value(5.0)));
    let add1 =
        ArithmeticExpression::Add(
            Box::new(ArithmeticExpression::Value(1.0)),
            Box::new(multiply));

    let add2 = ArithmeticExpression::Add(Box::new(add1), Box::new(divide));

    let ast = Expression::Arithmetic(add2);
    let expected = Ok(ast);

    assert_eq!(expected, parse(normal, expression));
}
