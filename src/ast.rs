use crate::object::{
    get_boolean_object, Environment, Error, Function, Integer, Object, ReturnValue, FALSE, NULL,
    TRUE,
};
use crate::token::Token;
use std::fmt;

pub trait ASTNode {
    fn evaluate(&self, environment: &mut Environment) -> Object;
}

#[derive(Debug, PartialEq, Clone)]
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
impl ASTNode for InfixExpression {
    fn evaluate(&self, environment: &mut Environment) -> Object {
        let left = self.left.evaluate(environment);
        if let Object::Error(_) = left {
            return left;
        }
        let right = self.right.evaluate(environment);
        if let Object::Error(_) = right {
            return right;
        }
        match left {
            Object::Integer(left_integer) => match right {
                Object::Integer(right_integer) => match self.operator.as_str() {
                    "+" => Object::Integer(Integer {
                        value: left_integer.value + right_integer.value,
                    }),
                    "-" => Object::Integer(Integer {
                        value: left_integer.value - right_integer.value,
                    }),
                    "*" => Object::Integer(Integer {
                        value: left_integer.value * right_integer.value,
                    }),
                    "/" => Object::Integer(Integer {
                        value: left_integer.value / right_integer.value,
                    }),
                    "<" => get_boolean_object(left_integer.value < right_integer.value),
                    ">" => get_boolean_object(left_integer.value > right_integer.value),
                    "==" => get_boolean_object(left_integer.value == right_integer.value),
                    "!=" => get_boolean_object(left_integer.value != right_integer.value),
                    _ => Object::Error(Error {
                        message: format!(
                            "unknown operator: {} {} {}",
                            left_integer.get_type(),
                            self.operator,
                            right_integer.get_type()
                        ),
                    }),
                },
                _ => Object::Error(Error {
                    message: format!(
                        "type mismatch: {} {} {}",
                        left_integer.get_type(),
                        self.operator,
                        right.get_type()
                    ),
                }),
            },
            Object::Boolean(left_boolean) => match right {
                Object::Boolean(right_boolean) => match self.operator.as_str() {
                    "==" => get_boolean_object(left_boolean.value == right_boolean.value),
                    "!=" => get_boolean_object(left_boolean.value != right_boolean.value),
                    _ => Object::Error(Error {
                        message: format!(
                            "unknown operator: {} {} {}",
                            left_boolean.get_type(),
                            self.operator,
                            right_boolean.get_type()
                        ),
                    }),
                },
                _ => Object::Error(Error {
                    message: format!(
                        "type mismatch: {} {} {}",
                        left_boolean.get_type(),
                        self.operator,
                        right.get_type()
                    ),
                }),
            },
            _ => Object::Error(Error {
                message: format!("unknown type: {}", left.get_type()),
            }),
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
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
impl ASTNode for PrefixExpression {
    fn evaluate(&self, environment: &mut Environment) -> Object {
        let right = self.right.evaluate(environment);
        if let Object::Error(_) = right {
            return right;
        }
        match self.operator.as_str() {
            "!" => match right {
                Object::Boolean(boolean) => get_boolean_object(!boolean.value),
                Object::Integer(integer) => {
                    if integer.value != 0 {
                        return FALSE;
                    } else {
                        return TRUE;
                    }
                }
                _ => FALSE,
            },
            "-" => match right {
                Object::Integer(integer) => {
                    return Object::Integer(Integer {
                        value: -integer.value,
                    });
                }
                _ => Object::Error(Error {
                    message: format!("unknown operator: {}{}", self.operator, right.get_type()),
                }),
            },
            _ => Object::Error(Error {
                message: format!("unknown operator: {}{}", self.operator, right.get_type()),
            }),
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct BooleanLiteral {
    pub token: Token,
    pub value: bool,
}
impl fmt::Display for BooleanLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl ASTNode for BooleanLiteral {
    fn evaluate(&self, _environment: &mut Environment) -> Object {
        get_boolean_object(self.value)
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}
impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
impl ASTNode for IntegerLiteral {
    fn evaluate(&self, _environment: &mut Environment) -> Object {
        return Object::Integer(Integer { value: self.value });
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
impl ASTNode for Identifier {
    fn evaluate(&self, environment: &mut Environment) -> Object {
        return match environment.get(&self.value) {
            Some(val) => val.clone(),
            None => Object::Error(Error {
                message: format!("identifier not found: {}", self.value),
            }),
        };
    }
}
#[derive(Debug, PartialEq, Clone)]
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
            "if ({}) {{{}}}{}",
            self.condition,
            self.consequence,
            match &self.alternative {
                Some(x) => format!(" else {{{}}}", x),
                None => "".to_string(),
            }
        )
    }
}
impl ASTNode for IfExpression {
    fn evaluate(&self, environment: &mut Environment) -> Object {
        fn is_truthy(evaluated_condition: Object) -> bool {
            match evaluated_condition {
                Object::Boolean(boolean) => boolean.value,
                Object::Null(_) => false,
                Object::Integer(integer) => {
                    match integer.value {
                        // Treat 0 as falsy
                        0 => false,
                        _ => true,
                    }
                }
                _ => true,
            }
        }

        let evaluated_condition = self.condition.evaluate(environment);
        if let Object::Error(_) = evaluated_condition {
            return evaluated_condition;
        }

        if is_truthy(evaluated_condition) {
            return self.consequence.evaluate(environment);
        }

        match &self.alternative {
            Some(alternative) => alternative.evaluate(environment),
            None => NULL,
        }
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct FunctionLiteral {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}
impl fmt::Display for FunctionLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string_parameters: Vec<String> =
            self.parameters.iter().map(|x| x.to_string()).collect();
        write!(f, "fn ({}) {{{}}}", string_parameters.join(", "), self.body)
    }
}
impl ASTNode for FunctionLiteral {
    fn evaluate(&self, environment: &mut Environment) -> Object {
        return Object::Function(Function {
            parameters: self.parameters.clone(),
            body: self.body.clone(),
            env: environment.clone(),
        });
    }
}
#[derive(Debug, PartialEq, Clone)]
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
impl ASTNode for CallExpression {
    fn evaluate(&self, environment: &mut Environment) -> Object {
        // evaluate function expression
        let evaluated_function = self.function.evaluate(environment);
        let evaluated_function = match evaluated_function {
            Object::Error(_) => {
                return evaluated_function;
            }
            Object::Function(function) => function,
            _ => {
                return Object::Error(Error {
                    message: "Expected to evaluate a function when evaluating a call expression"
                        .to_string(),
                });
            }
        };

        // Evaluate arguments, left to right
        let mut args = vec![];
        for argument in self.arguments.iter() {
            let evaluated_arg = argument.evaluate(environment);
            if let Object::Error(_) = evaluated_arg {
                return evaluated_arg;
            }
            args.push(evaluated_arg);
        }

        // set up function environment
        // TODO: Instead of copying the entire env, implement copy-on-write semantics.
        let mut function_env = evaluated_function.env.clone();
        for (parameter, argument) in evaluated_function.parameters.iter().zip(args.iter()) {
            function_env.set(parameter.value.clone(), argument.clone());
        }

        // evaluate function body
        let evaluated_body = evaluated_function.body.evaluate(&mut function_env);
        return match evaluated_body {
            Object::ReturnValue(return_value) => *return_value.value,
            _ => evaluated_body,
        };
    }
}
#[derive(Debug, PartialEq, Clone)]
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
impl ASTNode for Expression {
    fn evaluate(&self, environment: &mut Environment) -> Object {
        match self {
            Expression::Identifier(expression) => expression.evaluate(environment),
            Expression::IntegerLiteral(expression) => expression.evaluate(environment),
            Expression::BooleanLiteral(expression) => expression.evaluate(environment),
            Expression::PrefixExpression(expression) => expression.evaluate(environment),
            Expression::InfixExpression(expression) => expression.evaluate(environment),
            Expression::IfExpression(expression) => expression.evaluate(environment),
            Expression::FunctionLiteral(expression) => expression.evaluate(environment),
            Expression::CallExpression(expression) => expression.evaluate(environment),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Expression,
}
impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression)
    }
}
impl ASTNode for ExpressionStatement {
    fn evaluate(&self, environment: &mut Environment) -> Object {
        return self.expression.evaluate(environment);
    }
}
#[derive(Debug, PartialEq, Clone)]
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
impl ASTNode for LetStatement {
    fn evaluate(&self, environment: &mut Environment) -> Object {
        let evaluated = self.value.evaluate(environment);
        if let Object::Error(_) = evaluated {
            return evaluated;
        }

        return environment.set(self.name.value.clone(), evaluated);
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Expression,
}
impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {};", self.token.literal, self.return_value)
    }
}
impl ASTNode for ReturnStatement {
    fn evaluate(&self, environment: &mut Environment) -> Object {
        let evaluated = self.return_value.evaluate(environment);
        if let Object::Error(_) = evaluated {
            return evaluated;
        }
        return Object::ReturnValue(ReturnValue {
            value: Box::new(evaluated),
        });
    }
}
#[derive(Debug, PartialEq, Clone)]
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
impl ASTNode for BlockStatement {
    fn evaluate(&self, environment: &mut Environment) -> Object {
        let mut result = None;
        for statement in self.statements.iter() {
            result = Some(statement.evaluate(environment));

            match result {
                Some(Object::ReturnValue(_)) => {
                    return result.unwrap();
                }
                Some(Object::Error(_)) => {
                    return result.unwrap();
                }
                _ => (),
            }
        }
        return result.unwrap_or(NULL);
    }
}
#[derive(Debug, PartialEq, Clone)]
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
impl ASTNode for Statement {
    fn evaluate(&self, environment: &mut Environment) -> Object {
        match self {
            Statement::LetStatement(statement) => statement.evaluate(environment),
            Statement::ReturnStatement(statement) => statement.evaluate(environment),
            Statement::ExpressionStatement(statement) => statement.evaluate(environment),
            Statement::BlockStatement(statement) => statement.evaluate(environment),
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
impl ASTNode for Program {
    fn evaluate(&self, environment: &mut Environment) -> Object {
        let mut result = None;
        for statement in self.statements.iter() {
            result = Some(statement.evaluate(environment));

            match result {
                Some(Object::ReturnValue(return_value)) => {
                    return *return_value.value;
                }
                Some(Object::Error(_)) => {
                    return result.unwrap();
                }
                _ => (),
            }
        }
        return result.expect("Program is empty!");
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
