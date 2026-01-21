use peg::*;
use crate::parser::ParserError;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'a> {
    Identifier(&'a str),
    Int(i64),
    OpenParen,
    CloseParen,
    Equals,
    Plus,
    Minus,
    Comma,
    Newline,
}

parser! {
    grammar tokenizer() for str {
        rule _ = [' ' | '\t']* // Whitespace
        rule __ = !['A'..='Z' | 'a'..='z' | '_' | '0'..='9'] // Word token boundary
        rule end_of_file() = _ ![_] // Whitespace then EOF

        rule newline() -> Token<'input> = "\n" { Token::Newline }

        rule identifier() -> Token<'input>
            = name:$(['A'..='Z' | 'a'..='z' | '_']['A'..='Z' | 'a'..='z' | '_' | '0'..='9']*)
            { Token::Identifier(name) }

        rule int() -> Token<'input>
            = n:$("-"?['0'..='9']+)
            {? n.parse().or(Err("int")).and_then(|val| Ok(Token::Int(val))) }

        rule open_paren() -> Token<'input> = "(" { Token::OpenParen }
        rule close_paren() -> Token<'input> = ")" { Token::CloseParen }
        rule equals() -> Token<'input> = "=" { Token::Equals }
        rule plus() -> Token<'input> = "+" { Token::Plus }
        rule minus() -> Token<'input> = "-" { Token::Minus }
        rule comma() -> Token<'input> = "," { Token::Comma }

        /// A word token requires trailing whitespace/EOF
        rule word_token() -> Token<'input>
            = t:(identifier() / int()) &__ {t}

        /// A punctuation token does not require trailing whitespace
        rule punctuation_token() -> Token<'input>
            = open_paren() / close_paren() / equals() / plus() / minus() / comma() / newline()

        rule token() -> Token<'input> = word_token() / punctuation_token()

        /// Optionally-whitespace-delimited Tokens
        pub rule tokens() -> Vec<Token<'input>>
            = _ ts:token() ** (_?) end_of_file() { ts }
    }

}

pub fn tokenize<'a>(input: &'a str) -> Result<Vec<Token<'a>>, ParserError<'a>> {
    tokenizer::tokens(input).map_err(|e| {
            let got = input.chars().nth(e.location.offset).unwrap_or('~');
            ParserError::Tokenizer(e, got)
        })
}
