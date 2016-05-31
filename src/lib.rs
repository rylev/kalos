#[macro_use]
extern crate nom;
use nom::*;
use nom::IResult::*;

#[derive(PartialEq, Clone, Debug)]
pub enum Expression<'a> {
    Value(f64),
    Add(Box<Expression<'a>>, Box<Expression<'a>>),
    Subtract(Box<Expression<'a>>, Box<Expression<'a>>),
    Multiply(Box<Expression<'a>>, Box<Expression<'a>>),
    Divide(Box<Expression<'a>>, Box<Expression<'a>>),
    Enclosed(Box<Expression<'a>>),
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

named!(f64<f64>, map_res!(map_res!(digit, std::str::from_utf8), |n: &str| n.parse()));
named!(float<Expression>,
       chain!(
           opt!(space) ~
           val: map!(f64, Expression::Value) ~
           opt!(space),
           || val));
named!(enclosed_expression<Expression>,
    delimited!(
        chain!(opt!(space) ~ c: char!('(') ~ opt!(space),|| c),
        alt!(low_precedence),
        chain!(opt!(space) ~ c: char!(')') ~ opt!(space),|| c)));
named!(lhs<Expression>,
       alt_complete!(float | enclosed_expression));

named!(multiplication<(Operator, Expression)>,
    chain!(tag!("*") ~ mul: lhs, || (Operator::Multiplication, mul)));
named!(division<(Operator, Expression)>,
    chain!(tag!("/") ~ div: lhs, || (Operator::Division, div)));
named!(high_precedence<Expression>, chain!(
        initial: lhs ~
        remainder: many0!(alt!(multiplication | division)),
        || fold_exprs(initial, remainder)));

named!(addition<(Operator, Expression)>,
       chain!(tag!("+") ~ add: high_precedence, || (Operator::Addition, add)));
named!(subtraction<(Operator, Expression)>,
       chain!(tag!("-") ~ sub: high_precedence, || (Operator::Subtraction, sub)));
named!(low_precedence<Expression>, chain!(
    initial: high_precedence ~
    remainder: many0!(alt!(addition | subtraction)),
    || fold_exprs(initial, remainder)));

fn fold_exprs<'a>(initial: Expression<'a>, remainder: Vec<(Operator, Expression<'a>)>) -> Expression<'a> {
    remainder.into_iter().fold(initial, |acc, pair| {
        match pair {
            (Operator::Addition, expr) => Expression::Add(Box::new(acc), Box::new(expr)),
            (Operator::Subtraction, expr) => Expression::Subtract(Box::new(acc), Box::new(expr)),
            (Operator::Multiplication, expr) => Expression::Multiply(Box::new(acc), Box::new(expr)),
            (Operator::Division, expr) => Expression::Divide(Box::new(acc), Box::new(expr)),
        }
    })
}

named!(arithmetic_expression<Expression>, map!(low_precedence, |exp| exp));

named!(expression<Expression>,
    complete!(
        chain!(
            exp: alt_complete!(arithmetic_expression),
            || exp)));

named!(delimiter, tag!(";"));

named!(identifier<&str>, map_res!(take_while!(is_alphabetic), std::str::from_utf8));
named!(func_name<&str>, map!(identifier,|n| n));

named!(func_args<Vec<&str> >,
    chain!(
        char!('(') ~
        space? ~
        args: chain!(
            first: opt!(identifier) ~
            mut others: many0!(chain!(char!(',') ~ space? ~ arg: identifier, || arg)),
            || {
                first.map(|id| {
                    let mut args = vec!(id);
                    args.append(&mut others);
                    args
                }).unwrap_or(vec!())
            }) ~
        space? ~
        char!(')'),
        || args));

named!(func_body<Expression>,
    chain!(
        char!('{') ~
        expr: expression ~
        char!('}'),
        || expr));

named!(function<Function>,
    chain!(
        tag!("def") ~
        space ~
        name: func_name ~
        args: func_args ~
        space? ~
        body: func_body,
        || Function::new(name, args, body)));

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

impl<'a> Function<'a> {
    fn new(name: &'a str, args: Vec<&'a str>, body: Expression<'a>) -> Function<'a> {
        Function {
            prototype: Box::new(Prototype{name: name, args: args}),
            body: Box::new(body)
        }
    }
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
            Expression::Divide(
                Box::new(Expression::Value(1.0)),
                Box::new(Expression::Value(10.0))));
    let rhs =
        Box::new(
            Expression::Multiply(
                Box::new(Expression::Value(90.0)),
                Box::new(Expression::Value(67.0))));

    let ast = Expression::Add(lhs, rhs);
    let expected = Ok(ast);

    assert_eq!(expected, parse(normal, expression));
    assert_eq!(expected, parse(condensed, expression));
    assert_eq!(expected, parse(weird, expression));
}

#[test]
fn it_parses_chained_arithmetic() {
    let normal = "1 + 3 * 5 + 2 / 5";
    let divide =
        Expression::Divide(
            Box::new(Expression::Value(2.0)),
            Box::new(Expression::Value(5.0)));
    let multiply =
        Expression::Multiply(
            Box::new(Expression::Value(3.0)),
            Box::new(Expression::Value(5.0)));
    let add1 =
        Expression::Add(
            Box::new(Expression::Value(1.0)),
            Box::new(multiply));

    let ast = Expression::Add(Box::new(add1), Box::new(divide));

    let expected = Ok(ast);

    assert_eq!(expected, parse(normal, expression));
}

#[test]
fn it_parses_functions() {
    let normal = "def foo(bar, baz) { 1 }";
    let compact = "def foo(bar,baz){1}";
    let loose = "def foo( bar,    baz   )    {      1       }";
    let expected = Ok(Function::new("foo", vec!("bar", "baz"), Expression::Value(1.0)));
    assert_eq!(expected, parse(normal, function));
    assert_eq!(expected, parse(compact, function));
    assert_eq!(expected, parse(loose, function));

    let squashed = "deffoo( boo,    bar   ){ 1 } ";
    assert!(parse(squashed, function).is_err());
    let squashed = "def foo (boo, bar) { 1 }";
    assert!(parse(squashed, function).is_err());
}
