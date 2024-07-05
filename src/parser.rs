use crate::ast::{
    BlockStatement, BooleanLiteral, CallExpression, Expression, ExpressionStatement,
    FunctionLiteral, Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement,
    PrefixExpression, Program, ReturnStatement, Statement,
};
use crate::lexer::Lexer;
use crate::token::{self, Token, TokenType};
use std::collections::HashMap;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
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
    pub fn new(lexer: Lexer) -> Parser {
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

        // Register prefix parse functions
        parser
            .prefix_parse_fns
            .insert(TokenType::Identifier, Parser::parse_identifier);
        parser
            .prefix_parse_fns
            .insert(TokenType::Int, Parser::parse_integer_literal);
        parser
            .prefix_parse_fns
            .insert(TokenType::True, Parser::parse_boolean_literal);
        parser
            .prefix_parse_fns
            .insert(TokenType::False, Parser::parse_boolean_literal);
        parser
            .prefix_parse_fns
            .insert(TokenType::Bang, Parser::parse_prefix_expression);
        parser
            .prefix_parse_fns
            .insert(TokenType::Minus, Parser::parse_prefix_expression);
        parser
            .prefix_parse_fns
            .insert(TokenType::Lparen, Parser::parse_grouped_expression);
        parser
            .prefix_parse_fns
            .insert(TokenType::If, Parser::parse_if_expression);
        parser
            .prefix_parse_fns
            .insert(TokenType::Function, Parser::parse_function_literal);

        // Register infix parse functions
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
        parser
            .infix_parse_fns
            .insert(TokenType::Lparen, Parser::parse_call_expression);
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
        self.errors.push(format!(
            "Expected token type {:?} but got {:?}",
            token_type, self.peek_token.token_type,
        ));
        return false;
    }

    pub fn parse_program(&mut self) -> Program {
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

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest);

        if self.peek_token.token_type == TokenType::Semicolon {
            self.next_token();
        }

        return LetStatement {
            token: first_token,
            name: name,
            value: value,
        };
    }

    fn parse_return_statement(&mut self) -> ReturnStatement {
        let first_token = self.current_token.clone();

        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest);

        if self.peek_token.token_type == TokenType::Semicolon {
            self.next_token();
        }

        return ReturnStatement {
            token: first_token,
            return_value: return_value,
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

    fn parse_block_statement(&mut self) -> BlockStatement {
        let current_token = self.current_token.clone();

        self.next_token();

        let mut statements = vec![];
        while self.current_token.token_type != TokenType::Rbrace
            && self.current_token.token_type != TokenType::Eof
        {
            let statement = self.parse_statement();
            statements.push(statement);
            self.next_token();
        }

        return BlockStatement {
            token: current_token,
            statements: statements,
        };
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

    fn parse_boolean_literal(&mut self) -> Expression {
        return Expression::BooleanLiteral(BooleanLiteral {
            token: self.current_token.clone(),
            value: self.current_token.token_type == TokenType::True,
        });
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

    fn parse_grouped_expression(&mut self) -> Expression {
        self.next_token();

        let expression = self.parse_expression(Precedence::Lowest);

        self.expect_peek(TokenType::Rparen);

        return expression;
    }

    fn parse_if_expression(&mut self) -> Expression {
        let current_token = self.current_token.clone();
        self.expect_peek(TokenType::Lparen);

        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest);

        self.expect_peek(TokenType::Rparen);
        self.expect_peek(TokenType::Lbrace);

        let consequence = self.parse_block_statement();

        let mut alternative = None;
        if self.peek_token.token_type == TokenType::Else {
            self.next_token();
            self.expect_peek(TokenType::Lbrace);
            alternative = Some(self.parse_block_statement());
        }

        return Expression::IfExpression(IfExpression {
            token: current_token,
            condition: Box::new(condition),
            consequence: consequence,
            alternative: alternative,
        });
    }

    fn parse_function_literal(&mut self) -> Expression {
        let current_token = self.current_token.clone();
        self.expect_peek(TokenType::Lparen);

        let parameters = self.parse_function_parameters();

        self.expect_peek(TokenType::Lbrace);

        let body = self.parse_block_statement();

        return Expression::FunctionLiteral(FunctionLiteral {
            token: current_token,
            parameters: parameters,
            body: body,
        });
    }

    fn parse_function_parameters(&mut self) -> Vec<Identifier> {
        let mut parameters = vec![];

        if self.peek_token.token_type == TokenType::Rparen {
            self.next_token();
            return parameters;
        }

        self.next_token();

        parameters.push(Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        });

        while self.peek_token.token_type == TokenType::Comma {
            self.next_token();
            self.next_token();
            parameters.push(Identifier {
                token: self.current_token.clone(),
                value: self.current_token.literal.clone(),
            });
        }

        self.expect_peek(TokenType::Rparen);

        return parameters;
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

    fn parse_call_expression(&mut self, function: Expression) -> Expression {
        return Expression::CallExpression(CallExpression {
            token: self.current_token.clone(),
            function: Box::new(function),
            arguments: self.parse_call_arguments(),
        });
    }

    fn parse_call_arguments(&mut self) -> Vec<Expression> {
        let mut arguments = vec![];

        if self.peek_token.token_type == TokenType::Rparen {
            self.next_token();
            return arguments;
        }

        self.next_token();
        arguments.push(self.parse_expression(Precedence::Lowest));

        while self.peek_token.token_type == TokenType::Comma {
            self.next_token();
            self.next_token();
            arguments.push(self.parse_expression(Precedence::Lowest));
        }

        self.expect_peek(TokenType::Rparen);

        return arguments;
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{
        BlockStatement, BooleanLiteral, CallExpression, Expression, ExpressionStatement,
        FunctionLiteral, Identifier, IfExpression, InfixExpression, IntegerLiteral, LetStatement,
        ReturnStatement, Statement,
    };
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::token::{Token, TokenType};

    /*
     * Helper functions for testing
     */

    fn check_parser_errors(parser: Parser) {
        assert_eq!(
            parser.errors.len(),
            0,
            "Parser found {} errors!",
            parser.errors.len()
        );
    }

    fn build_return_statement(value: Expression) -> Statement {
        return Statement::ReturnStatement(ReturnStatement {
            token: Token {
                token_type: TokenType::Return,
                literal: "return".to_string(),
            },
            return_value: value,
        });
    }

    fn build_let_statement(name: Expression, value: Expression) -> Statement {
        return Statement::LetStatement(LetStatement {
            token: Token {
                token_type: TokenType::Let,
                literal: "let".to_string(),
            },
            name: match name {
                Expression::Identifier(identifier) => identifier,
                _ => panic!("name is not an identifier"),
            },
            value: value,
        });
    }

    fn build_identifier(value: &str) -> Identifier {
        return Identifier {
            token: Token {
                token_type: TokenType::Identifier,
                literal: value.to_string(),
            },
            value: value.to_string(),
        };
    }

    fn build_integer_literal_expression(value: i64) -> Expression {
        return Expression::IntegerLiteral(IntegerLiteral {
            token: Token {
                token_type: TokenType::Int,
                literal: value.to_string(),
            },
            value: value,
        });
    }

    fn build_boolean_literal_expression(value: bool) -> Expression {
        return Expression::BooleanLiteral(BooleanLiteral {
            token: Token {
                token_type: if value {
                    TokenType::True
                } else {
                    TokenType::False
                },
                literal: if value {
                    "true".to_string()
                } else {
                    "false".to_string()
                },
            },
            value: value,
        });
    }

    fn build_if_expression(
        condition: Expression,
        consequence: Statement,
        alternative: Option<Statement>,
    ) -> Expression {
        return Expression::IfExpression(IfExpression {
            token: Token {
                token_type: TokenType::If,
                literal: "if".to_string(),
            },
            condition: Box::new(condition),
            consequence: match consequence {
                Statement::BlockStatement(consequence) => consequence,
                _ => panic!("consequence is not a BlockStatement"),
            },
            alternative: match alternative {
                Some(Statement::BlockStatement(alternative)) => Some(alternative),
                None => None,
                _ => panic!("alternative is not a BlockStatement"),
            },
        });
    }

    fn build_infix_expression(left: Expression, token: Token, right: Expression) -> Expression {
        return Expression::InfixExpression(InfixExpression {
            token: token.clone(),
            left: Box::new(left),
            operator: token.literal,
            right: Box::new(right),
        });
    }

    fn build_function_literal_expression(
        parameters: Vec<Identifier>,
        body: BlockStatement,
    ) -> Expression {
        return Expression::FunctionLiteral(FunctionLiteral {
            token: Token {
                token_type: TokenType::Function,
                literal: "fn".to_string(),
            },
            parameters: parameters,
            body: body,
        });
    }

    fn build_block_statement(statements: Vec<Statement>) -> BlockStatement {
        return BlockStatement {
            token: Token {
                token_type: TokenType::Lbrace,
                literal: "{".to_string(),
            },
            statements: statements,
        };
    }

    /*
     * Tests
     */

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

        let expected_statements = vec![
            build_let_statement(
                Expression::Identifier(build_identifier("x")),
                build_integer_literal_expression(5),
            ),
            build_let_statement(
                Expression::Identifier(build_identifier("y")),
                build_integer_literal_expression(10),
            ),
            build_let_statement(
                Expression::Identifier(build_identifier("foobar")),
                build_integer_literal_expression(838383),
            ),
        ];
        for (i, statement) in program.statements.iter().enumerate() {
            assert_eq!(*statement, expected_statements[i],);
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

        let expected_statements = vec![
            build_return_statement(build_integer_literal_expression(5)),
            build_return_statement(build_integer_literal_expression(10)),
            build_return_statement(build_integer_literal_expression(993322)),
        ];
        for (i, statement) in program.statements.iter().enumerate() {
            assert_eq!(*statement, expected_statements[i]);
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
            assert_eq!(
                *statement,
                Statement::ExpressionStatement(ExpressionStatement {
                    token: build_identifier("foobar").token,
                    expression: Expression::Identifier(build_identifier("foobar"))
                })
            );
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
            assert_eq!(
                *statement,
                Statement::ExpressionStatement(ExpressionStatement {
                    token: build_integer_literal_expression(5).get_token().clone(),
                    expression: build_integer_literal_expression(5),
                }),
            );
        }
    }

    #[test]
    fn test_boolean_literal_expression() {
        let input = "true;";

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
            assert_eq!(
                *statement,
                Statement::ExpressionStatement(ExpressionStatement {
                    token: build_boolean_literal_expression(true).get_token().clone(),
                    expression: build_boolean_literal_expression(true),
                })
            );
        }
    }

    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";

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
            assert_eq!(
                *statement,
                Statement::ExpressionStatement(ExpressionStatement {
                    token: Token {
                        token_type: TokenType::If,
                        literal: "if".to_string()
                    },
                    expression: build_if_expression(
                        build_infix_expression(
                            Expression::Identifier(build_identifier("x")),
                            Token {
                                token_type: TokenType::LessThan,
                                literal: "<".to_string()
                            },
                            Expression::Identifier(build_identifier("y")),
                        ),
                        Statement::BlockStatement(build_block_statement(vec![
                            Statement::ExpressionStatement(ExpressionStatement {
                                token: build_identifier("x").token,
                                expression: Expression::Identifier(build_identifier("x"))
                            })
                        ])),
                        None,
                    )
                })
            );
        }
    }

    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";

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
            assert_eq!(
                *statement,
                Statement::ExpressionStatement(ExpressionStatement {
                    token: Token {
                        token_type: TokenType::If,
                        literal: "if".to_string(),
                    },
                    expression: build_if_expression(
                        build_infix_expression(
                            Expression::Identifier(build_identifier("x")),
                            Token {
                                token_type: TokenType::LessThan,
                                literal: "<".to_string(),
                            },
                            Expression::Identifier(build_identifier("y")),
                        ),
                        Statement::BlockStatement(build_block_statement(vec![
                            Statement::ExpressionStatement(ExpressionStatement {
                                token: build_identifier("x").token,
                                expression: Expression::Identifier(build_identifier("x"))
                            })
                        ])),
                        Some(Statement::BlockStatement(build_block_statement(vec![
                            Statement::ExpressionStatement(ExpressionStatement {
                                token: build_identifier("y").token,
                                expression: Expression::Identifier(build_identifier("y"))
                            })
                        ]))),
                    )
                })
            );
        }
    }

    #[test]
    fn test_function_literal_expressions() {
        struct TestData {
            input: String,
            expected_output: Statement,
        }
        let tests = vec![
            TestData {
                input: "fn() {};".to_string(),
                expected_output: Statement::ExpressionStatement(ExpressionStatement {
                    token: Token {
                        token_type: TokenType::Function,
                        literal: "fn".to_string(),
                    },
                    expression: build_function_literal_expression(
                        vec![],
                        build_block_statement(vec![]),
                    ),
                }),
            },
            TestData {
                input: "fn(x) {};".to_string(),
                expected_output: Statement::ExpressionStatement(ExpressionStatement {
                    token: Token {
                        token_type: TokenType::Function,
                        literal: "fn".to_string(),
                    },
                    expression: build_function_literal_expression(
                        vec![build_identifier("x")],
                        build_block_statement(vec![]),
                    ),
                }),
            },
            TestData {
                input: "fn(x, y, z) {};".to_string(),
                expected_output: Statement::ExpressionStatement(ExpressionStatement {
                    token: Token {
                        token_type: TokenType::Function,
                        literal: "fn".to_string(),
                    },
                    expression: build_function_literal_expression(
                        vec![
                            build_identifier("x"),
                            build_identifier("y"),
                            build_identifier("z"),
                        ],
                        build_block_statement(vec![]),
                    ),
                }),
            },
            TestData {
                input: "fn(x, y) { x + y; }".to_string(),
                expected_output: Statement::ExpressionStatement(ExpressionStatement {
                    token: Token {
                        token_type: TokenType::Function,
                        literal: "fn".to_string(),
                    },
                    expression: build_function_literal_expression(
                        vec![build_identifier("x"), build_identifier("y")],
                        build_block_statement(vec![Statement::ExpressionStatement(
                            ExpressionStatement {
                                token: build_identifier("x").token,
                                expression: build_infix_expression(
                                    Expression::Identifier(build_identifier("x")),
                                    Token {
                                        token_type: TokenType::Plus,
                                        literal: "+".to_string(),
                                    },
                                    Expression::Identifier(build_identifier("y")),
                                ),
                            },
                        )]),
                    ),
                }),
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
                assert_eq!(*statement, test.expected_output,);
            }
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";

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
            assert_eq!(
                *statement,
                Statement::ExpressionStatement(ExpressionStatement {
                    token: Token {
                        token_type: TokenType::Identifier,
                        literal: "add".to_string(),
                    },
                    expression: Expression::CallExpression(CallExpression {
                        token: Token {
                            token_type: TokenType::Lparen,
                            literal: "(".to_string(),
                        },
                        function: Box::new(Expression::Identifier(build_identifier("add")),),
                        arguments: vec![
                            build_integer_literal_expression(1),
                            build_infix_expression(
                                build_integer_literal_expression(2),
                                Token {
                                    token_type: TokenType::Asterisk,
                                    literal: "*".to_string(),
                                },
                                build_integer_literal_expression(3)
                            ),
                            build_infix_expression(
                                build_integer_literal_expression(4),
                                Token {
                                    token_type: TokenType::Plus,
                                    literal: "+".to_string(),
                                },
                                build_integer_literal_expression(5)
                            )
                        ],
                    })
                })
            );
        }
    }

    #[test]
    fn test_prefix_expressions() {
        struct TestData {
            input: String,
            operator: String,
            expression: Expression,
        }
        let tests = vec![
            TestData {
                input: "!5;".to_string(),
                operator: "!".to_string(),
                expression: build_integer_literal_expression(5),
            },
            TestData {
                input: "-5;".to_string(),
                operator: "-".to_string(),
                expression: build_integer_literal_expression(5),
            },
            TestData {
                input: "!true".to_string(),
                operator: "!".to_string(),
                expression: build_boolean_literal_expression(true),
            },
            TestData {
                input: "!false".to_string(),
                operator: "!".to_string(),
                expression: build_boolean_literal_expression(false),
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
                            assert_eq!(*expression.right, test.expression);
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
            left: Expression,
            operator: String,
            right: Expression,
        }
        let tests = vec![
            TestData {
                input: "5 + 5;".to_string(),
                left: build_integer_literal_expression(5),
                operator: "+".to_string(),
                right: build_integer_literal_expression(5),
            },
            TestData {
                input: "5 - 5;".to_string(),
                left: build_integer_literal_expression(5),
                operator: "-".to_string(),
                right: build_integer_literal_expression(5),
            },
            TestData {
                input: "5 * 5;".to_string(),
                left: build_integer_literal_expression(5),
                operator: "*".to_string(),
                right: build_integer_literal_expression(5),
            },
            TestData {
                input: "5 / 5;".to_string(),
                left: build_integer_literal_expression(5),
                operator: "/".to_string(),
                right: build_integer_literal_expression(5),
            },
            TestData {
                input: "5 > 5;".to_string(),
                left: build_integer_literal_expression(5),
                operator: ">".to_string(),
                right: build_integer_literal_expression(5),
            },
            TestData {
                input: "5 < 5;".to_string(),
                left: build_integer_literal_expression(5),
                operator: "<".to_string(),
                right: build_integer_literal_expression(5),
            },
            TestData {
                input: "5 == 5;".to_string(),
                left: build_integer_literal_expression(5),
                operator: "==".to_string(),
                right: build_integer_literal_expression(5),
            },
            TestData {
                input: "5 != 5;".to_string(),
                left: build_integer_literal_expression(5),
                operator: "!=".to_string(),
                right: build_integer_literal_expression(5),
            },
            TestData {
                input: "true == true".to_string(),
                left: build_boolean_literal_expression(true),
                operator: "==".to_string(),
                right: build_boolean_literal_expression(true),
            },
            TestData {
                input: "false == false".to_string(),
                left: build_boolean_literal_expression(false),
                operator: "==".to_string(),
                right: build_boolean_literal_expression(false),
            },
            TestData {
                input: "false != true".to_string(),
                left: build_boolean_literal_expression(false),
                operator: "!=".to_string(),
                right: build_boolean_literal_expression(true),
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
                            assert_eq!(*expression.left, test.left);
                            assert_eq!(expression.operator, test.operator);
                            assert_eq!(*expression.right, test.right);
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
            TestData {
                input: "true".to_string(),
                expected: "true".to_string(),
            },
            TestData {
                input: "false".to_string(),
                expected: "false".to_string(),
            },
            TestData {
                input: "3 > 5 == false".to_string(),
                expected: "((3 > 5) == false)".to_string(),
            },
            TestData {
                input: "3 < 5 == true".to_string(),
                expected: "((3 < 5) == true)".to_string(),
            },
            TestData {
                input: "1 + (2 + 3) + 4".to_string(),
                expected: "((1 + (2 + 3)) + 4)".to_string(),
            },
            TestData {
                input: "(5 + 5) * 2".to_string(),
                expected: "((5 + 5) * 2)".to_string(),
            },
            TestData {
                input: "2 / (5 + 5)".to_string(),
                expected: "(2 / (5 + 5))".to_string(),
            },
            TestData {
                input: "-(5 + 5)".to_string(),
                expected: "(-(5 + 5))".to_string(),
            },
            TestData {
                input: "!(true == true)".to_string(),
                expected: "(!(true == true))".to_string(),
            },
            TestData {
                input: "a + add(b * c) + d".to_string(),
                expected: "((a + add((b * c))) + d)".to_string(),
            },
            TestData {
                input: "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))".to_string(),
                expected: "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))".to_string(),
            },
            TestData {
                input: "add(a + b + c * d / f + g)".to_string(),
                expected: "add((((a + b) + ((c * d) / f)) + g))".to_string(),
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
