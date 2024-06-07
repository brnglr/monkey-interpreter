use crate::token::Token;
use std::fmt;

#[derive(Debug, PartialEq)]
pub struct InfixExpression {
    pub token: Token,
    pub left: Box<Expression>,
    pub operator: String,
    pub right: Box<Expression>,
}
impl fmt::Display for InfixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}
#[derive(Debug, PartialEq)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub right: Box<Expression>,
}
impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}
#[derive(Debug, PartialEq)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}
impl fmt::Display for BooleanLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
#[derive(Debug, PartialEq)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}
impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct Identifier {
    pub token: Token,
    pub value: String,
}
impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
#[derive(Debug, PartialEq)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}
impl fmt::Display for IfExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "if ({}) {}{}",
            self.condition,
            self.consequence,
            match &self.alternative {
                Some(x) => format!(" else {}", x),
                None => "".to_string(),
            }
        )
    }
}
#[derive(Debug, PartialEq)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}
impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string_parameters: Vec<String> =
            self.parameters.iter().map(|x| x.to_string()).collect();
        write!(f, "fn ({}) {}", string_parameters.join(", "), self.body)
    }
}
#[derive(Debug, PartialEq)]
pub struct CallExpression {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}
impl fmt::Display for CallExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string_arguments: Vec<String> = self.arguments.iter().map(|x| x.to_string()).collect();
        write!(f, "{}({})", self.function, string_arguments.join(", "))
    }
}
#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    BooleanLiteral(BooleanLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
    IfExpression(IfExpression),
    FunctionLiteral(FunctionLiteral),
    CallExpression(CallExpression),
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Identifier(expression) => {
                write!(f, "{}", expression)
            }
            Expression::IntegerLiteral(expression) => {
                write!(f, "{}", expression)
            }
            Expression::BooleanLiteral(expression) => {
                write!(f, "{}", expression)
            }
            Expression::PrefixExpression(expression) => {
                write!(f, "{}", expression)
            }
            Expression::InfixExpression(expression) => {
                write!(f, "{}", expression)
            }
            Expression::IfExpression(expression) => {
                write!(f, "{}", expression)
            }
            Expression::FunctionLiteral(expression) => {
                write!(f, "{}", expression)
            }
            Expression::CallExpression(expression) => {
                write!(f, "{}", expression)
            }
        }
    }
}
impl Expression {
    pub fn get_token(&self) -> &Token {
        match self {
            Expression::Identifier(expression) => &expression.token,
            Expression::IntegerLiteral(expression) => &expression.token,
            Expression::BooleanLiteral(expression) => &expression.token,
            Expression::PrefixExpression(expression) => &expression.token,
            Expression::InfixExpression(expression) => &expression.token,
            Expression::IfExpression(expression) => &expression.token,
            Expression::FunctionLiteral(expression) => &expression.token,
            Expression::CallExpression(expression) => &expression.token,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}
impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}
#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Expression,
}
impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} = {};", self.token.literal, self.name, self.value)
    }
}
#[derive(Debug, PartialEq)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}
impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {};", self.token.literal, self.return_value)
    }
}
#[derive(Debug, PartialEq)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}
impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for statement in self.statements.iter() {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}
#[derive(Debug, PartialEq)]
pub enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
    BlockStatement(BlockStatement),
}
impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::LetStatement(statement) => {
                write!(f, "{}", statement)
            }
            Statement::ReturnStatement(statement) => {
                write!(f, "{}", statement)
            }
            Statement::ExpressionStatement(statement) => {
                write!(f, "{}", statement)
            }
            Statement::BlockStatement(statement) => {
                write!(f, "{}", statement)
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}
impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for statement in self.statements.iter() {
            write!(f, "{}", statement)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Identifier, LetStatement, Program, Statement};
    use crate::token::{Token, TokenType};

    #[test]
    fn test_to_string() {
        let program = Program {
            statements: vec![Statement::LetStatement(LetStatement {
                token: Token {
                    token_type: TokenType::Let,
                    literal: "let".to_string(),
                },
                name: Identifier {
                    token: Token {
                        token_type: TokenType::Identifier,
                        literal: "myVar".to_string(),
                    },
                    value: "myVar".to_string(),
                },
                value: Expression::Identifier(Identifier {
                    token: Token {
                        token_type: TokenType::Identifier,
                        literal: "anotherVar".to_string(),
                    },
                    value: "anotherVar".to_string(),
                }),
            })],
        };

        assert_eq!(
            program.to_string(),
            "let myVar = anotherVar;".to_string(),
            "to_string produced invalid program output",
        )
    }
}
