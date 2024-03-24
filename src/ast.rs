use crate::token::Token;

#[derive(Debug)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}
#[derive(Debug)]
pub enum Expression {
    Identifier(Identifier),
}

#[derive(Debug)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}
#[derive(Debug)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}
#[derive(Debug)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
}

pub struct Program {
    pub statements: Vec<Statement>,
}
