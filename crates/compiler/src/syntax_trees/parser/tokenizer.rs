use peg::*;

use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take_while},
    combinator::{all_consuming, eof, peek, recognize},
    multi::many0,
    sequence::terminated,
};
use nom_locate::LocatedSpan;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenValue<'a> {
    Identifier(&'a str),
    Int(i64),
    Bool(bool),
    OpenParen,
    CloseParen,
    Equals,
    Plus,
    Minus,
    Comma,
    Newline,
    DoubleEquals,
    NotEquals,
    Greater,
    GreaterEquals,
    Less,
    LessEquals,
    Not,
    And,
    Or,
    If,
    Else,
    OpenCurly,
    CloseCurly,
    QuestionMark,
    Colon,
    While,
    Is,
    OpenBracket,
    CloseBracket,
    Asterisk,
    Fn,
    IntType,
    BoolType,
    TupleType,
    ArrayType,
    CallableType,
    NoneType,
    RightArrow,
    Return,
    Lambda,
    For,
    Semicolon,
}

#[derive(Debug)]
pub struct Token<'a> {
    pub token: TokenValue<'a>,
    pub span: LocatedSpan<&'a str>,
}

const ID_START_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_";
const WORD_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_";

macro_rules! token_map {
    ($string: expr, $tok: expr) => {
        tag($string).map(|s| (s, $tok))
    };
}

fn word_delimiter(rem: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, ()> {
    peek(is_not(WORD_CHARS)).map(|_| ()).parse(rem)
}

fn newline<'a>(rem: LocatedSpan<&'a str>) -> IResult<LocatedSpan<&'a str>, Token<'a>> {
    let (rem, span) = is_a("\r\n").parse(rem)?;

    Ok((
        rem,
        Token {
            token: TokenValue::Newline,
            span,
        },
    ))
}

fn whitespace(input: LocatedSpan<&str>) -> IResult<LocatedSpan<&str>, ()> {
    alt((tag(" "), tag("\t"))).map(|_| ()).parse(input)
}

fn keyword_parser<'a>(rem: LocatedSpan<&'a str>) -> IResult<LocatedSpan<&'a str>, Token<'a>> {
    use TokenValue::*;

    let (rem, (span, token)) = terminated(
        alt((
            token_map!("true", Bool(true)),
            token_map!("false", Bool(false)),
            token_map!("if", If),
            token_map!("else", Else),
            token_map!("while", While),
            token_map!("for", For),
            token_map!("and", And),
            token_map!("or", Or),
            token_map!("not", Not),
            token_map!("is", Is),
            token_map!("fn", Fn),
            token_map!("int", IntType),
            token_map!("bool", BoolType),
            token_map!("tuple", TupleType),
            token_map!("array", ArrayType),
            token_map!("callable", CallableType),
            token_map!("none", NoneType),
            token_map!("return", Return),
            token_map!("lambda", Lambda),
        )),
        word_delimiter,
    )
    .parse(rem)?;

    Ok((rem, Token { token, span }))
}

fn punctuation_parser<'a>(rem: LocatedSpan<&'a str>) -> IResult<LocatedSpan<&'a str>, Token<'a>> {
    use TokenValue::*;

    // let (rem, _) = position(input)?;
    let (rem, (span, token)) = alt((
        alt((
            token_map!("->", RightArrow),
            token_map!("==", DoubleEquals),
            token_map!("!=", NotEquals),
            token_map!(">=", GreaterEquals),
            token_map!("<=", LessEquals),
            token_map!("&&", And),
            token_map!("||", Or),
            token_map!(">", Greater),
            token_map!("<", Less),
            token_map!("!", Not),
            token_map!("(", OpenParen),
            token_map!(")", CloseParen),
            token_map!("=", Equals),
            token_map!("+", Plus),
            token_map!("-", Minus),
            token_map!(",", Comma),
            token_map!("{", OpenCurly),
            token_map!("}", CloseCurly),
            token_map!("[", OpenBracket),
            token_map!("]", CloseBracket),
            token_map!("?", QuestionMark),
        )), // Each `alt` supports a max of 21 choices
        alt((
            token_map!(":", Colon),
            token_map!("*", Asterisk),
            token_map!(";", Semicolon),
        )),
    ))
    .parse(rem)?;

    Ok((rem, Token { token, span }))
}

fn int_parser(rem: LocatedSpan<&'_ str>) -> IResult<LocatedSpan<&'_ str>, Token<'_>> {
    let (rem, token_span) =
        recognize(terminated(nom::character::complete::i64, word_delimiter)).parse(rem)?;

    let int_val = token_span
        .clone()
        .into_fragment()
        .parse::<i64>()
        .expect("Couldn't parse i64");
    let token = TokenValue::Int(int_val);

    Ok((
        rem,
        Token {
            token,
            span: token_span,
        },
    ))
}

fn id_parser(rem: LocatedSpan<&'_ str>) -> IResult<LocatedSpan<&'_ str>, Token<'_>> {
    let (rem, id_span) =
        terminated(take_while(|c| WORD_CHARS.contains(c)), word_delimiter).parse(rem)?;

    // Make sure the first char is a valid start char
    let _ = is_a(ID_START_CHARS)(id_span)?;

    Ok((
        rem,
        Token {
            token: TokenValue::Identifier(id_span.clone().into_fragment()),
            span: id_span,
        },
    ))
}

fn token_parser(rem: LocatedSpan<&'_ str>) -> IResult<LocatedSpan<&'_ str>, Token<'_>> {
    let (rem, _) = many0(whitespace).parse(rem)?;

    let tok = alt((
        newline,
        keyword_parser,
        int_parser,
        punctuation_parser,
        id_parser,
    ))
    .parse(rem);

    tok
}

pub fn tokenize(
    input: &'_ str,
) -> Result<Vec<Token<'_>>, nom::Err<nom::error::Error<LocatedSpan<&'_ str>>>> {
    let res = all_consuming((many0(token_parser), eof)).parse(LocatedSpan::new(input));

    if let Ok((rem, (tokens, _))) = res {
        assert!(rem.is_empty());
        Ok(tokens)
    } else if let Err(err) = res {
        Err(err)
    } else {
        unreachable!()
    }
}
