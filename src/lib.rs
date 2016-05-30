#[macro_use]
extern crate nom;
use nom::*;
use nom::IResult::*;


#[derive(PartialEq, Clone, Debug)]
pub enum Expression<'a> {
    Literal(Literal),
    Variable(&'a str),
    BinaryExpression(Box<Expression<'a>>, Operator, Box<Expression<'a>>),
    FunctionCall(&'a str, Vec<Expression<'a>>)
}
named!(literal_expression(&[u8]) -> Expression,
    map!(literal, |l| Expression::Literal(l)));
named!(enclosed(&[u8]) -> Expression,
    delimited!(
        char!('('),
        expression,
        char!(')')));
named!(binary_expression(&[u8]) -> Expression,
    chain!(
        exp1: alt!(literal_expression | enclosed) ~
        op: operator ~
        exp2: alt!(literal_expression | enclosed),
        || Expression::BinaryExpression(Box::new(exp1), op, Box::new(exp2))));
named!(expression(&[u8]) -> Expression,
    alt_complete!(binary_expression | literal_expression));

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

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Float(f64),
}
named!(f64<f64>, map_res!(map_res!(digit, std::str::from_utf8), |n: &str| n.parse()));
named!(float<Literal>, map!(f64, Literal::Float));
named!(literal<Literal>,
       alt!(float));

#[derive(PartialEq, Clone, Debug)]
pub enum Operator {
    Addition,
    Subtraction,
    Multiplication,
    Division
}

named!(plus<Operator>, map!(tag!("+"), |_| Operator::Addition));
named!(minus<Operator>, map!(tag!("-"), |_| Operator::Subtraction));
named!(multiply<Operator>, map!(tag!("*"), |_| Operator::Multiplication));
named!(divide<Operator>, map!(tag!("/"), |_| Operator::Division));
named!(operator<Operator>,
    alt!(plus | minus | multiply | divide));

#[derive(PartialEq, Debug)]
pub enum ParserError<'a> {
    NotFinished(&'a str),
    FailedParse,
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
        Error(_) => Err(ParserError::FailedParse),
        _ => Err(ParserError::WTF),
    }
}

#[test]
fn it_parses_math() {
    let math = "(1/10)+(90*67)";
    let lhs =
        Box::new(
            Expression::BinaryExpression(
                Box::new(Expression::Literal(Literal::Float(1.0))),
                Operator::Division,
                Box::new(Expression::Literal(Literal::Float(10.0)))));
    let rhs =
        Box::new(
            Expression::BinaryExpression(
                Box::new(Expression::Literal(Literal::Float(90.0))),
                Operator::Multiplication,
                Box::new(Expression::Literal(Literal::Float(67.0)))));

    let ast = Expression::BinaryExpression(lhs, Operator::Addition, rhs);
    let expected = Ok(ast);
    let actual = parse(math, expression);
    assert_eq!(expected, actual);
}
