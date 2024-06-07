use crate::parser::Precedence;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum TokenType {
    Illegal,
    Eof,

    // Identifiers + literals
    Identifier,
    Int,

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Equal,
    NotEqual,
    LessThan,
    GreaterThan,

    // Delimiters
    Comma,
    Semicolon,

    Lparen, // (
    Rparen, // )
    Lbrace, // {
    Rbrace, // }

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

pub fn get_precedence(token_type: &TokenType) -> Precedence {
    match token_type {
        TokenType::Lparen => Precedence::Call,
        TokenType::Equal => Precedence::Equals,
        TokenType::NotEqual => Precedence::Equals,
        TokenType::LessThan => Precedence::LessGreater,
        TokenType::GreaterThan => Precedence::LessGreater,
        TokenType::Plus => Precedence::Sum,
        TokenType::Minus => Precedence::Sum,
        TokenType::Slash => Precedence::Product,
        TokenType::Asterisk => Precedence::Product,
        _ => Precedence::Lowest,
    }
}

pub fn is_allowed_in_identifier(character: u8) -> bool {
    return (b'a'..=b'z').contains(&character)
        | (b'A'..=b'Z').contains(&character)
        | (character == b'_');
}

pub fn is_allowed_in_int(character: u8) -> bool {
    return character.is_ascii_digit();
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}
