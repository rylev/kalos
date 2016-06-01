#[macro_use]
extern crate nom;
use nom::*;
use nom::IResult::*;

#[derive(PartialEq, Clone, Debug)]
pub enum Token<'a> {
    Def,
    Extern,
    Delimiter,
    OpenParen,
    ClosedParen,
    OpenBrace,
    ClosedBrace,
    Comma,
    EndOfLine,
    Identifier(&'a str),
    Number(f64),
    Operator(Operator)
}
#[derive(PartialEq, Clone, Debug)]
pub enum Operator {
    Addition,
    Subtraction,
    Multiplication,
    Division
}

impl Operator {
    fn precedence(&self) -> Precedence {
        match self {
            &Operator::Addition => Precedence::AddSub,
            &Operator::Subtraction => Precedence::AddSub,
            &Operator::Multiplication => Precedence::MultDiv,
            &Operator::Division => Precedence::MultDiv
        }
    }
}

named!(def<Token>, map!(chain!(tag!("def") ~ space, || ()), |_| Token::Def));
named!(delimiter<Token>, map!(char!(';'), |_| Token::Delimiter));
named!(open_paren<Token>, map!(char!('('), |_| Token::OpenParen));
named!(closed_paren<Token>, map!(char!(')'), |_| Token::ClosedParen));
named!(open_brace<Token>, map!(char!('{'), |_| Token::OpenBrace));
named!(closed_brace<Token>, map!(char!('}'), |_| Token::ClosedBrace));
named!(comma<Token>, map!(char!(','), |_| Token::Comma));
named!(f64<f64>, map_res!(map_res!(digit, std::str::from_utf8), |n: &str| n.parse()));
named!(number<Token>, map!(f64, |n| Token::Number(n)));
// TODO: improve this to be more strict. e.g. not allow 'HELLO' as identifier
// named!(end_of_line<Token>, map!(alt!(eof | tag!(";") | tag!("\n")), |_| Token::EndOfLine));
named!(identifier<Token>, map!(map_res!(take_while1!(is_alphabetic), std::str::from_utf8), |s| Token::Identifier(s)));
named!(plus<Operator>, map!(tag!("+"), |_| Operator::Addition));
named!(minus<Operator>, map!(tag!("-"), |_| Operator::Subtraction));
named!(multiply<Operator>, map!(tag!("*"), |_| Operator::Multiplication));
named!(divide<Operator>, map!(tag!("/"), |_| Operator::Division));
named!(operator<Token>, map!(alt_complete!(plus | minus | multiply | divide), |o| Token::Operator(o)));

named!(token<Token>,
    chain!(
        space? ~
        token: alt!(
            def |
            number |
            // extern_token |
            delimiter |
            open_paren |
            closed_paren |
            open_brace |
            closed_brace |
            comma |
            identifier |
            operator),
            || token));

named!(tokens<Vec<Token> >, many0!(token));

pub fn tokenize<'a>(input: &'a str) -> Result<Vec<Token>, String> {
    match tokens(input.as_bytes()) {
        Done(leftover, _) if !leftover.is_empty() => {
            // TODO: remove unwrap
            Err(format!("Left over input '{}'", std::str::from_utf8(leftover).unwrap()))
        }
        Done(_, result) => Ok(result),
        Error(Err::Position(parser, unparseable)) => {
            println!("Could not parse at {:?}", parser);
            // TODO: remove unwrap
            Err(format!("Could not parse '{}'", std::str::from_utf8(unparseable).unwrap()))
        }
        e => Err(format!("Unhandled Error: {:?}", e)),
    }
}

#[derive(PartialEq, Clone, Debug)]
pub enum ASTNode<'a> {
    FuncDeclartion(Function<'a>)
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expression<'a> {
    Value(f64),
    Add(Box<Expression<'a>>, Box<Expression<'a>>),
    Subtract(Box<Expression<'a>>, Box<Expression<'a>>),
    Multiply(Box<Expression<'a>>, Box<Expression<'a>>),
    Divide(Box<Expression<'a>>, Box<Expression<'a>>),
    Variable(&'a str),
    FunctionCall(&'a str, Vec<Expression<'a>>)
}

#[derive(PartialEq, Clone, Debug)]
pub struct Prototype<'a>  {
    name: &'a str,
    args: Vec<&'a str>
}

impl<'a> Prototype<'a> {
    fn new(name: &'a str, args: Vec<&'a str>) -> Prototype<'a> {
        Prototype{name: name, args: args}
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Function<'a> {
    prototype: Box<Prototype<'a>>,
    body: Box<Expression<'a>>
}

impl<'a> Function<'a> {
    fn new(name: &'a str, args: Vec<&'a str>, body: Expression<'a>) -> Function<'a> {
        Function {
            prototype: Box::new(Prototype::new(name, args)),
            body: Box::new(body)
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum ParserError {
    UnexpectedToken(String),
    UnexpectedEndOfInput,
}

#[macro_export]
macro_rules! try_parse {
    ($expr:expr) => (match $expr {
        Ok(parsed) => parsed,
        Err(err) => return Err(err)
    })
}
macro_rules! next_token {
    ($tokens:ident) => (match $tokens.pop() {
        Some(token) => token,
        None        => return Err(ParserError::UnexpectedEndOfInput)
    })
}
macro_rules! eat_token {
    ($tokens:ident) => (match $tokens.pop() {
        Some(_) => {},
        None        => return Err(ParserError::UnexpectedEndOfInput)
    })
}
macro_rules! peek_token {
    ($tokens:ident) => (match $tokens.last() {
        Some(token) => token,
        None        => return Err(ParserError::UnexpectedEndOfInput)
    })
}
macro_rules! assert_next_token {
    ($tokens:ident, $token_pattern:pat) => (match next_token!($tokens) {
        token@$token_pattern => token,
        other                => return unexpected_token(other, &[]) // TODO: Figure out what to put as the second arg
    })
}

fn unexpected_token<T>(token: Token, expected_tokens: &[&str]) -> ParseResult<T> {
    let mut expected_tokens_string = String::new();
    for (i, expected_token) in expected_tokens.iter().enumerate() {
        if i == 0 {
            expected_tokens_string.push_str(&format!("{:?}", expected_token))
        } else if i == expected_tokens.len() - 1 {
            expected_tokens_string.push_str(&format!(", or {:?}", expected_token))
        } else {
            expected_tokens_string.push_str(&format!(", {:?}", expected_token))
        }
    }
    let message = format!("Expected {}, found {:?}", expected_tokens_string, token);
    Err(ParserError::UnexpectedToken(message))
}

pub type ParseResult<T> = Result<T, ParserError>;

#[derive(Debug, PartialEq)]
enum Precedence {
    None,
    AddSub,
    MultDiv
}
use std::cmp::{Ordering,PartialOrd};
impl PartialOrd<Precedence> for Precedence {
     fn partial_cmp(&self, other: &Precedence) -> Option<Ordering> {
         let order = match (self, other) {
             (&Precedence::None,    &Precedence::None)    => Ordering::Equal,
             (&Precedence::AddSub,  &Precedence::AddSub)  => Ordering::Equal,
             (&Precedence::MultDiv, &Precedence::MultDiv) => Ordering::Equal,
             (&Precedence::MultDiv, &Precedence::AddSub)  => Ordering::Greater,
             (&Precedence::AddSub,  &Precedence::MultDiv) => Ordering::Less,
             (&Precedence::None, _)                       => Ordering::Less,
             (_, &Precedence::None)                       => Ordering::Greater,
         };
         Some(order)
     }
}

pub fn parse_expression<'a>(tokens: &mut Vec<Token<'a>>) -> ParseResult<Expression<'a>> {
    let lhs = try_parse!(parse_primary(tokens));
    parse_binary_expression(tokens, &lhs, Precedence::None)
}

fn parse_primary<'a>(tokens: &mut Vec<Token<'a>>) -> ParseResult<Expression<'a>> {
    match next_token!(tokens) {
        Token::Number(n) => parse_number_expression(tokens, n),
        Token::OpenParen => parse_paren_expression(tokens),
        Token::Identifier(name) => parse_identifier_expression(tokens, name),
        other => unexpected_token(other, &["number", "(", "identifier"])
    }
}

fn parse_identifier_expression<'a>(tokens: &mut Vec<Token<'a>>, name: &'a str) -> ParseResult<Expression<'a>> {
    if let &Token::OpenParen = peek_token!(tokens) {
        eat_token!(tokens);
        let args = try_parse!(parse_function_call_args(tokens));
        Ok(Expression::FunctionCall(name, args))
    } else {
        Ok(Expression::Variable(name))
    }
}

fn parse_number_expression<'a>(_tokens: &mut Vec<Token<'a>>, n: f64) -> ParseResult<Expression<'a>> {
    Ok(Expression::Value(n))
}

fn parse_paren_expression<'a>(tokens: &mut Vec<Token<'a>>) -> ParseResult<Expression<'a>> {
    let exp = try_parse!(parse_expression(tokens));
    assert_next_token!(tokens, Token::ClosedParen);
    Ok(exp)
}

fn parse_function_call_args<'a>(tokens: &mut Vec<Token<'a>>) -> ParseResult<Vec<Expression<'a>>> {
    let mut args = vec!();
    if let &Token::ClosedParen = peek_token!(tokens) {
        eat_token!(tokens);
        Ok(args)
    } else {
        loop {
            args.push(try_parse!(parse_expression(tokens)));
            match next_token!(tokens) {
                Token::Comma       => continue,
                Token::ClosedParen => return Ok(args),
                other              => return unexpected_token(other, &[",", ")"])
            }
        }
    }
}

fn parse_binary_expression<'a>(tokens: &mut Vec<Token<'a>>, lhs: &Expression<'a>, current_precedence: Precedence) -> ParseResult<Expression<'a>> {
    let mut lhs = lhs.clone();
    loop {
        //  * an operator with higher precedence than current precedence, bind to operator
        //  * otherwise we have gone far enough, our current precedence is the top of current
        //  subtree, return lhs
        let operator = match peek_token!(tokens) {
            &Token::Operator(ref next_op) if next_op.precedence() > current_precedence => next_op.clone(),
            _                                                                          => return Ok(lhs),
        };
        // Eat the operator we just peeked at
        let _ = eat_token!(tokens);

        // parse expression right of operator
        let mut rhs = try_parse!(parse_primary(tokens));
        // keep going until sub binary expression has an operator that has lower
        // precedence than "operator"
        rhs = try_parse!(parse_binary_expression(tokens, &rhs, operator.precedence()));

        // Put lhs and rhs together to form a new lhs
        lhs = match operator {
            Operator::Addition => Expression::Add(Box::new(lhs), Box::new(rhs)),
            Operator::Subtraction => Expression::Subtract(Box::new(lhs), Box::new(rhs)),
            Operator::Multiplication => Expression::Multiply(Box::new(lhs), Box::new(rhs)),
            Operator::Division => Expression::Divide(Box::new(lhs), Box::new(rhs)),
        };
    }
}

pub fn parse_function_definition<'a>(tokens: &mut Vec<Token<'a>>) -> ParseResult<Function<'a>> {
    match next_token!(tokens) {
        Token::Identifier(name) => {
            let args = try_parse!(parse_function_definition_args(tokens));
            assert_next_token!(tokens, Token::OpenBrace);
            // TODO: multiline
            let expression = try_parse!(parse_expression(tokens));
            Ok(Function::new(name, args, expression))
        },
        other => unexpected_token(other, &["identifier"])
    }
}

fn parse_function_definition_args<'a>(tokens: &mut Vec<Token<'a>>) -> ParseResult<Vec<&'a str>> {
    assert_next_token!(tokens, Token::OpenParen);
    let mut args = vec!();
    while let Token::Identifier(id) = next_token!(tokens) {
        args.push(id);
        match next_token!(tokens) {
            Token::ClosedParen => break,
            Token::Comma       => {},
            other              => return unexpected_token(other, &[")", ","]),
        }
    }
    Ok(args)
}

#[test]
fn it_tokenizes_functions() {
    let func = "def foo(bar, baz) { 1 }";
    let expected = vec!(
        Token::Def,
        Token::Identifier("foo"),
        Token::OpenParen,
        Token::Identifier("bar"),
        Token::Comma,
        Token::Identifier("baz"),
        Token::ClosedParen,
        Token::OpenBrace,
        Token::Number(1.0),
        Token::ClosedBrace);
    assert_eq!(Ok(expected), tokenize(func));
}

#[test]
fn it_tokenizes_functions_with_reserved_words_in_identifier() {
    let func = "def define()";
    let expected = vec!(
        Token::Def,
        Token::Identifier("define"),
        Token::OpenParen,
        Token::ClosedParen);
    assert_eq!(Ok(expected), tokenize(func));
}

#[test]
fn it_tokenizes_arthimetic() {
    println!("{:?}", identifier("+".as_bytes()));
    let math = "1 + 3 * 5 + 2 / 5";
    let expected = vec!(
        Token::Number(1.0),
        Token::Operator(Operator::Addition),
        Token::Number(3.0),
        Token::Operator(Operator::Multiplication),
        Token::Number(5.0),
        Token::Operator(Operator::Addition),
        Token::Number(2.0),
        Token::Operator(Operator::Division),
        Token::Number(5.0));
    assert_eq!(Ok(expected), tokenize(math));
}

#[test]
fn it_parses_functions() {
    let mut func = vec!(
        // Token::Def,
        Token::Identifier("foo"),
        Token::OpenParen,
        Token::Identifier("bar"),
        Token::Comma,
        Token::Identifier("baz"),
        Token::ClosedParen,
        Token::OpenBrace,
        Token::Number(1.0),
        Token::ClosedBrace);
    func.reverse();

    let expected = Function::new("foo", vec!("bar", "baz"), Expression::Value(1.0));
    assert_eq!(Ok(expected), parse_function_definition(&mut func));
}

#[test]
fn it_handles_malformed_functions() {
    let mut func = vec!(
        // Token::Def,
        Token::Identifier("foo"),
        Token::OpenParen,
        Token::Identifier("bar"),
        Token::OpenBrace,
        Token::Number(1.0),
        Token::ClosedBrace);
    func.reverse();

    assert!(parse_function_definition(&mut func).is_err());
}

#[test]
fn it_parses_arthimetic() {
    let mut math = vec!(
        Token::Number(1.0),
        Token::Operator(Operator::Addition),
        Token::Number(3.0),
        Token::Operator(Operator::Multiplication),
        Token::Number(5.0),
        Token::Operator(Operator::Addition),
        Token::Number(2.0),
        Token::Operator(Operator::Division),
        Token::Number(5.0),
        Token::EndOfLine);
    math.reverse();

    let multiply =
        Expression::Multiply(
            Box::new(Expression::Value(3.0)),
            Box::new(Expression::Value(5.0)));
    let add =
        Expression::Add(
            Box::new(Expression::Value(1.0)),
            Box::new(multiply));
    let divide =
        Expression::Divide(
            Box::new(Expression::Value(2.0)),
            Box::new(Expression::Value(5.0)));

    let ast = Expression::Add(Box::new(add), Box::new(divide));

    let expected = Ok(ast);
    assert_eq!(expected, parse_expression(&mut math));
}

#[test]
fn it_parses_grouped_arthimetic() {
    let mut math = vec!(
        Token::OpenParen,
        Token::Number(1.0),
        Token::Operator(Operator::Addition),
        Token::Number(3.0),
        Token::ClosedParen,
        Token::Operator(Operator::Multiplication),
        Token::Number(5.0),
        Token::Operator(Operator::Addition),
        Token::Number(2.0),
        Token::Operator(Operator::Division),
        Token::Number(5.0),
        Token::EndOfLine);
    math.reverse();

    let add1 =
        Expression::Add(
            Box::new(Expression::Value(1.0)),
            Box::new(Expression::Value(3.0)));
    let multiply =
        Expression::Multiply(
            Box::new(add1),
            Box::new(Expression::Value(5.0)));
    let divide =
        Expression::Divide(
            Box::new(Expression::Value(2.0)),
            Box::new(Expression::Value(5.0)));

    let ast = Expression::Add(Box::new(multiply), Box::new(divide));

    let expected = Ok(ast);
    assert_eq!(expected, parse_expression(&mut math));
}

#[test]
fn it_parses_function_calls() {
    let mut call = vec!(
        Token::Identifier("foo"),
        Token::OpenParen,
        Token::Number(1.0),
        Token::Operator(Operator::Addition),
        Token::Number(3.0),
        Token::Comma,
        Token::Number(5.0),
        Token::Operator(Operator::Multiplication),
        Token::Number(2.0),
        Token::ClosedParen,
        Token::EndOfLine);
    call.reverse();

    let add =
        Expression::Add(
            Box::new(Expression::Value(1.0)),
            Box::new(Expression::Value(3.0)));
    let multiply =
        Expression::Multiply(
            Box::new(Expression::Value(5.0)),
            Box::new(Expression::Value(2.0)));

    let ast = Expression::FunctionCall("foo", vec!(add, multiply));

    let expected = Ok(ast);
    assert_eq!(expected, parse_expression(&mut call));
}
