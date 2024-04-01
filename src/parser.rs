use crate::ast::{
    Expression, ExpressionStatement, Identifier, InfixExpression, IntegerLiteral, LetStatement,
    PrefixExpression, Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::{self, Token, TokenType};
use std::collections::HashMap;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, for<'a> fn(&'a mut Parser) -> Expression>,
    infix_parse_fns: HashMap<TokenType, for<'a> fn(&'a mut Parser, Expression) -> Expression>,
}

#[derive(PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest = 1,
    Equals = 2,
    LessGreater = 3,
    Sum = 4,
    Product = 5,
    Prefix = 6,
    Call = 7,
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer: lexer,
            current_token: Token {
                token_type: TokenType::Illegal,
                literal: "".to_string(),
            },
            peek_token: Token {
                token_type: TokenType::Illegal,
                literal: "".to_string(),
            },
            errors: Vec::new(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        parser.next_token();
        parser.next_token();

        parser
            .prefix_parse_fns
            .insert(TokenType::Identifier, Parser::parse_identifier);
        parser
            .prefix_parse_fns
            .insert(TokenType::Int, Parser::parse_integer_literal);
        parser
            .prefix_parse_fns
            .insert(TokenType::Bang, Parser::parse_prefix_expression);
        parser
            .prefix_parse_fns
            .insert(TokenType::Minus, Parser::parse_prefix_expression);
        parser
            .infix_parse_fns
            .insert(TokenType::Plus, Parser::parse_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenType::Minus, Parser::parse_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenType::Slash, Parser::parse_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenType::Asterisk, Parser::parse_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenType::Equal, Parser::parse_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenType::NotEqual, Parser::parse_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenType::LessThan, Parser::parse_infix_expression);
        parser
            .infix_parse_fns
            .insert(TokenType::GreaterThan, Parser::parse_infix_expression);
        return parser;
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.peek_token.token_type == token_type {
            self.next_token();
            return true;
        }
        self.errors.push(
            "Expected token type {token_type} but got {self.peek_token.token_type}".to_string(),
        );
        return false;
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.current_token.token_type != TokenType::Eof {
            let statement = self.parse_statement();
            program.statements.push(statement);
            self.next_token();
        }
        return program;
    }

    fn parse_statement(&mut self) -> Statement {
        match self.current_token.token_type {
            TokenType::Let => {
                return Statement::LetStatement(self.parse_let_statement());
            }
            TokenType::Return => {
                return Statement::ReturnStatement(self.parse_return_statement());
            }
            _ => {
                return Statement::ExpressionStatement(self.parse_expression_statement());
            }
        }
    }

    fn parse_let_statement(&mut self) -> LetStatement {
        let first_token = self.current_token.clone();

        self.expect_peek(TokenType::Identifier);

        let name = Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };

        self.expect_peek(TokenType::Assign);

        // TODO: DELETE THIS ONCE WE HAVE EXPRESSION PARSING
        while self.current_token.token_type != TokenType::Semicolon {
            self.next_token();
        }
        let dummy_object = Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };
        return LetStatement {
            token: first_token,
            name: name,
            value: Expression::Identifier(dummy_object),
        };
    }

    fn parse_return_statement(&mut self) -> ReturnStatement {
        let first_token = self.current_token.clone();

        self.next_token();

        // TODO: DELETE THIS ONCE WE HAVE EXPRESSION PARSING
        while self.current_token.token_type != TokenType::Semicolon {
            self.next_token();
        }
        let dummy_object = Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };

        return ReturnStatement {
            token: first_token,
            return_value: Expression::Identifier(dummy_object),
        };
    }

    fn parse_expression_statement(&mut self) -> ExpressionStatement {
        let statement = ExpressionStatement {
            token: self.current_token.clone(),
            expression: self.parse_expression(Precedence::Lowest),
        };

        if self.peek_token.token_type == TokenType::Semicolon {
            self.next_token();
        }

        return statement;
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Expression {
        let prefix_fn = self.prefix_parse_fns[&self.current_token.token_type];
        let mut left_expression = prefix_fn(self);

        while self.peek_token.token_type != TokenType::Semicolon
            && precedence < token::get_precedence(&self.peek_token.token_type)
        {
            if !self
                .infix_parse_fns
                .contains_key(&self.peek_token.token_type)
            {
                return left_expression;
            }

            let infix = self.infix_parse_fns[&self.peek_token.token_type];
            self.next_token();
            left_expression = infix(self, left_expression);
        }

        return left_expression;
    }

    fn parse_identifier(&mut self) -> Expression {
        return Expression::Identifier(Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        });
    }

    fn parse_integer_literal(&mut self) -> Expression {
        match self.current_token.literal.parse::<i64>() {
            Err(_) => {
                self.errors
                    .push("Could not parse {self.current_token.literal} as i64".to_string());
                return Expression::IntegerLiteral(IntegerLiteral {
                    token: self.current_token.clone(),
                    value: 0,
                });
            }
            Ok(value) => {
                return Expression::IntegerLiteral(IntegerLiteral {
                    token: self.current_token.clone(),
                    value: value,
                });
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Expression {
        let operator_token = self.current_token.clone();

        self.next_token();

        return Expression::PrefixExpression(PrefixExpression {
            token: operator_token.clone(),
            operator: operator_token.literal,
            right: Box::new(self.parse_expression(Precedence::Prefix)),
        });
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Expression {
        let token = self.current_token.clone();

        let precedence = token::get_precedence(&self.current_token.token_type);
        self.next_token();
        let right_expression = self.parse_expression(precedence);

        return Expression::InfixExpression(InfixExpression {
            token: token.clone(),
            left: Box::new(left),
            operator: token.literal,
            right: Box::new(right_expression),
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Expression, Statement};
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn check_parser_errors(parser: Parser) {
        assert_eq!(
            parser.errors.len(),
            0,
            "Parser found {} errors!",
            parser.errors.len()
        );
    }

    fn test_statement(statement: &Statement, name: String) {
        match statement {
            Statement::LetStatement(statement) => {
                assert_eq!(statement.token.literal, "let");
                assert_eq!(statement.name.token.literal, name);
                assert_eq!(statement.name.value, name);
            }
            _ => panic!(
                "Statement was not parsed as a let statement: {:?}",
                statement
            ),
        }
    }

    #[test]
    fn test_let_statements() {
        let input = "let x = 5;
let y = 10;
let foobar = 838383;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(parser);
        assert_eq!(
            program.statements.len(),
            3,
            "Unexpected amount of statements parsed"
        );

        let expected_identifiers = vec!["x", "y", "foobar"];
        for (i, statement) in program.statements.iter().enumerate() {
            test_statement(statement, expected_identifiers[i].to_string());
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "return 5;
return 10;
return 993322;";
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        check_parser_errors(parser);
        assert_eq!(
            program.statements.len(),
            3,
            "Unexpected amount of statements parsed"
        );

        for statement in program.statements.iter() {
            match statement {
                Statement::ReturnStatement(statement) => {
                    assert_eq!(statement.token.literal, "return");
                }
                _ => panic!(
                    "Statement was not parsed as a return statement: {:?}",
                    statement
                ),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(parser);

        assert_eq!(
            program.statements.len(),
            1,
            "Unexpected amount of statements parsed"
        );

        for statement in program.statements.iter() {
            match statement {
                Statement::ExpressionStatement(statement) => match &statement.expression {
                    Expression::Identifier(expression) => {
                        assert_eq!(expression.value, "foobar".to_string());
                        assert_eq!(expression.token.literal, "foobar".to_string());
                    }
                    _ => panic!("Expression is not an identifier as expected!"),
                },
                _ => panic!("Statement is not an expression statement as expected!"),
            }
        }
    }

    fn test_integer_literal(expression: &Expression, value: i64) {
        match expression {
            Expression::IntegerLiteral(expression) => {
                assert_eq!(expression.value, value);
                assert_eq!(expression.token.literal, value.to_string());
            }
            _ => panic!("Expression is not an integer literal as expected!"),
        }
    }

    #[test]
    fn test_integer_literal_expression() {
        let input = "5;";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();

        check_parser_errors(parser);

        assert_eq!(
            program.statements.len(),
            1,
            "Unexpected amount of statements parsed"
        );

        for statement in program.statements.iter() {
            match statement {
                Statement::ExpressionStatement(statement) => {
                    test_integer_literal(&statement.expression, 5);
                }
                _ => panic!("Statement is not an expression statement as expected!"),
            }
        }
    }

    #[test]
    fn test_prefix_expressions() {
        struct TestData {
            input: String,
            operator: String,
            integer_value: i64,
        }
        let tests = vec![
            TestData {
                input: "!5;".to_string(),
                operator: "!".to_string(),
                integer_value: 5,
            },
            TestData {
                input: "-5;".to_string(),
                operator: "-".to_string(),
                integer_value: 5,
            },
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(&test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(parser);
            assert_eq!(
                program.statements.len(),
                1,
                "Unexpected amount of statements parsed"
            );

            for statement in program.statements.iter() {
                match statement {
                    Statement::ExpressionStatement(statement) => match &statement.expression {
                        Expression::PrefixExpression(expression) => {
                            assert_eq!(expression.operator, test.operator);
                            test_integer_literal(&expression.right, test.integer_value);
                        }
                        _ => panic!("Expression is not a prefix expression as expected!"),
                    },
                    _ => panic!("Statement is not an expression statement as expected!"),
                }
            }
        }
    }

    #[test]
    fn test_infix_expressions() {
        struct TestData {
            input: String,
            left: i64,
            operator: String,
            right: i64,
        }
        let tests = vec![
            TestData {
                input: "5 + 5;".to_string(),
                left: 5,
                operator: "+".to_string(),
                right: 5,
            },
            TestData {
                input: "5 - 5;".to_string(),
                left: 5,
                operator: "-".to_string(),
                right: 5,
            },
            TestData {
                input: "5 * 5;".to_string(),
                left: 5,
                operator: "*".to_string(),
                right: 5,
            },
            TestData {
                input: "5 / 5;".to_string(),
                left: 5,
                operator: "/".to_string(),
                right: 5,
            },
            TestData {
                input: "5 > 5;".to_string(),
                left: 5,
                operator: ">".to_string(),
                right: 5,
            },
            TestData {
                input: "5 < 5;".to_string(),
                left: 5,
                operator: "<".to_string(),
                right: 5,
            },
            TestData {
                input: "5 == 5;".to_string(),
                left: 5,
                operator: "==".to_string(),
                right: 5,
            },
            TestData {
                input: "5 != 5;".to_string(),
                left: 5,
                operator: "!=".to_string(),
                right: 5,
            },
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(&test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(parser);
            assert_eq!(
                program.statements.len(),
                1,
                "Unexpected amount of statements parsed"
            );

            for statement in program.statements.iter() {
                match statement {
                    Statement::ExpressionStatement(statement) => match &statement.expression {
                        Expression::InfixExpression(expression) => {
                            test_integer_literal(&expression.left, test.left);
                            assert_eq!(expression.operator, test.operator);
                            test_integer_literal(&expression.right, test.right);
                        }
                        _ => panic!("Expression is not a infix expression as expected!"),
                    },
                    _ => panic!("Statement is not an expression statement as expected!"),
                }
            }
        }
    }

    #[test]
    fn test_operator_precedence_parsing() {
        struct TestData {
            input: String,
            expected: String,
        }
        let tests = vec![
            TestData {
                input: "-a * b".to_string(),
                expected: "((-a) * b)".to_string(),
            },
            TestData {
                input: "!-a".to_string(),
                expected: "(!(-a))".to_string(),
            },
            TestData {
                input: "a + b + c".to_string(),
                expected: "((a + b) + c)".to_string(),
            },
            TestData {
                input: "a + b - c".to_string(),
                expected: "((a + b) - c)".to_string(),
            },
            TestData {
                input: "a * b * c".to_string(),
                expected: "((a * b) * c)".to_string(),
            },
            TestData {
                input: "a * b / c".to_string(),
                expected: "((a * b) / c)".to_string(),
            },
            TestData {
                input: "a + b / c".to_string(),
                expected: "(a + (b / c))".to_string(),
            },
            TestData {
                input: "a + b * c + d / e - f".to_string(),
                expected: "(((a + (b * c)) + (d / e)) - f)".to_string(),
            },
            TestData {
                input: "3 + 4; -5 * 5".to_string(),
                expected: "(3 + 4)((-5) * 5)".to_string(),
            },
            TestData {
                input: "5 > 4 == 3 < 4".to_string(),
                expected: "((5 > 4) == (3 < 4))".to_string(),
            },
            TestData {
                input: "5 < 4 != 3 > 4".to_string(),
                expected: "((5 < 4) != (3 > 4))".to_string(),
            },
            TestData {
                input: "3 + 4 * 5 == 3 * 1 + 4 * 5".to_string(),
                expected: "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))".to_string(),
            },
        ];

        for test in tests.iter() {
            let lexer = Lexer::new(&test.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            check_parser_errors(parser);
            assert_eq!(
                program.to_string(),
                test.expected,
                "Program parsed incorrectly"
            );
        }
    }
}
