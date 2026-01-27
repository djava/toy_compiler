use crate::parser::ParserError;
use peg::*;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token<'a> {
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
    OpenBracket,
    CloseBracket,
    QuestionMark,
    Colon,
}

parser! {
    grammar tokenizer() for str {
        rule _ = [' ' | '\t']* // Whitespace
        rule __ = !['A'..='Z' | 'a'..='z' | '_' | '0'..='9'] // Word token boundary
        rule end_of_file() = _ ![_] // Whitespace then EOF

        rule identifier() -> Token<'input>
            = name:$(['A'..='Z' | 'a'..='z' | '_']['A'..='Z' | 'a'..='z' | '_' | '0'..='9']*)
            { Token::Identifier(name) }

        rule int() -> Token<'input>
            = n:$("-"?['0'..='9']+)
            {? n.parse().or(Err("int")).and_then(|val| Ok(Token::Int(val))) }

        rule _true() -> Token<'input> = "true" &__ { Token::Bool(true) }
        rule _false() -> Token<'input> = "false" &__ { Token::Bool(false) }
        rule bool() -> Token<'input> = _true() / _false()

        rule double_equals() -> Token<'input> = "==" { Token::DoubleEquals }
        rule not_equals() -> Token<'input> = "!=" { Token::NotEquals }
        rule greater_equals() -> Token<'input> = ">=" { Token::GreaterEquals }
        rule less_equals() -> Token<'input> = "<=" { Token::LessEquals }
        rule greater() -> Token<'input> = ">" { Token::Greater }
        rule less() -> Token<'input> = "<" { Token::Less }
        rule and_sym() -> Token<'input> = "&&" { Token::And }
        rule or_sym() -> Token<'input> = "||" { Token::Or }
        rule not_sym() -> Token<'input> = "!" { Token::Not }
        rule open_paren() -> Token<'input> = "(" { Token::OpenParen }
        rule close_paren() -> Token<'input> = ")" { Token::CloseParen }
        rule equals() -> Token<'input> = "=" { Token::Equals }
        rule plus() -> Token<'input> = "+" { Token::Plus }
        rule minus() -> Token<'input> = "-" { Token::Minus }
        rule comma() -> Token<'input> = "," { Token::Comma }
        rule newline() -> Token<'input> = "\n" { Token::Newline }
        rule open_bracket() -> Token<'input> = "{" { Token::OpenBracket }
        rule close_bracket() -> Token<'input> = "}" { Token::CloseBracket }
        rule question_mark() -> Token<'input> = "?" { Token::QuestionMark }
        rule colon() -> Token<'input> = ":" { Token::Colon }

        rule _if() -> Token<'input> = "if" { Token::If }
        rule _else() -> Token<'input> = "else" { Token::Else }

        rule and_word() -> Token<'input> = "and" &__ { Token::And }
        rule or_word() -> Token<'input> = "or" &__ { Token::Or }
        rule not_word() -> Token<'input> = "not" &__ { Token::Not }

        /// A word token requires trailing whitespace/EOF/puncutation
        rule word_token() -> Token<'input>
            = t:(bool() / and_word() / or_word() / not_word() /
                 int() / _if() / _else() / identifier()) &__ {t}

        /// A punctuation token does not require trailing whitespace
        rule punctuation_token() -> Token<'input>
            = double_equals() / not_equals() / greater_equals() / less_equals() /
              greater() / less() / and_sym() / or_sym() / not_sym() / open_paren() /
              close_paren() / equals() / plus() / minus() / comma() / newline() /
              open_bracket() / close_bracket() / question_mark() / colon()

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
