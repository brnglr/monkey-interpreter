use crate::ast::{Expression, Identifier, LetStatement, Program, Statement};
use crate::lexer::Lexer;
use crate::token::{Token, TokenType};

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
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
        };
        parser.next_token();
        parser.next_token();
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
            _ => todo!(),
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

        while self.current_token.token_type != TokenType::Semicolon {
            self.next_token();
        }
        // TODO: DELETE THIS ONCE WE HAVE EXPRESSION PARSING
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
}

#[cfg(test)]
mod tests {
    use crate::ast::Statement;
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
}
