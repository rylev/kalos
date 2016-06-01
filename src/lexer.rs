use nom::*;
use nom::IResult::*;
use std;

#[derive(PartialEq, Clone, Debug)]
pub enum Token<'a> {
    Def,
    // Extern,
    Delimiter,
    OpenParen,
    ClosedParen,
    OpenBrace,
    ClosedBrace,
    Comma,
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
    pub fn precedence(&self) -> Precedence {
        match self {
            &Operator::Addition => Precedence::AddSub,
            &Operator::Subtraction => Precedence::AddSub,
            &Operator::Multiplication => Precedence::MultDiv,
            &Operator::Division => Precedence::MultDiv
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Precedence {
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
        Error(Err::Position(_, unparseable)) => {
            // TODO: remove unwrap
            Err(format!("Could not parse '{}'", std::str::from_utf8(unparseable).unwrap()))
        }
        e => Err(format!("Unhandled Error: {:?}", e)),
    }
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

