// TODO:
//     - Lexer is ASCII only. Could it be made Unicode compatible?
//     - Approach in Token, having is_allowed_in_*, won't work once
//       we introduce string literals.

pub mod lexer {
    pub mod token {
        #[derive(Debug, PartialEq)]
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

        pub fn is_allowed_in_identifier(character: u8) -> bool {
            return (b'a'..=b'z').contains(&character)
                | (b'A'..=b'Z').contains(&character)
                | (character == b'_');
        }

        pub fn is_allowed_in_int(character: u8) -> bool {
            return character.is_ascii_digit();
        }

        #[derive(Debug)]
        pub struct Token {
            pub token_type: TokenType,
            pub literal: String,
        }
    }
    use crate::lexer::token::{Token, TokenType};

    pub struct Lexer {
        pub input: Vec<u8>,
        position: usize,
        read_position: usize,
        current_char: u8,
    }

    impl Lexer {
        pub fn new(input: &str) -> Lexer {
            let mut lexer = Lexer {
                input: input.as_bytes().to_vec(),
                position: 0,
                read_position: 0,
                current_char: 0,
            };
            lexer.read_char();
            return lexer;
        }

        pub fn next_token(&mut self) -> Token {
            self.skip_whitespace();

            if token::is_allowed_in_identifier(self.current_char) {
                return self.read_identifier();
            }
            if token::is_allowed_in_int(self.current_char) {
                return self.read_number();
            }

            let next_token = match self.current_char {
                b'=' => {
                    if self.peek_char() == b'=' {
                        self.read_char();
                        Token {
                            token_type: TokenType::Equal,
                            literal: "==".to_string(),
                        }
                    } else {
                        Token {
                            token_type: TokenType::Assign,
                            literal: "=".to_string(),
                        }
                    }
                }
                b'+' => Token {
                    token_type: TokenType::Plus,
                    literal: "+".to_string(),
                },
                b'-' => Token {
                    token_type: TokenType::Minus,
                    literal: "-".to_string(),
                },
                b'!' => {
                    if self.peek_char() == b'=' {
                        self.read_char();
                        Token {
                            token_type: TokenType::NotEqual,
                            literal: "!=".to_string(),
                        }
                    } else {
                        Token {
                            token_type: TokenType::Bang,
                            literal: "!".to_string(),
                        }
                    }
                }
                b'*' => Token {
                    token_type: TokenType::Asterisk,
                    literal: "*".to_string(),
                },
                b'/' => Token {
                    token_type: TokenType::Slash,
                    literal: "/".to_string(),
                },
                b'<' => Token {
                    token_type: TokenType::LessThan,
                    literal: "<".to_string(),
                },
                b'>' => Token {
                    token_type: TokenType::GreaterThan,
                    literal: ">".to_string(),
                },
                b';' => Token {
                    token_type: TokenType::Semicolon,
                    literal: ";".to_string(),
                },
                b'(' => Token {
                    token_type: TokenType::Lparen,
                    literal: "(".to_string(),
                },
                b')' => Token {
                    token_type: TokenType::Rparen,
                    literal: ")".to_string(),
                },
                b',' => Token {
                    token_type: TokenType::Comma,
                    literal: ",".to_string(),
                },
                b'{' => Token {
                    token_type: TokenType::Lbrace,
                    literal: "{".to_string(),
                },
                b'}' => Token {
                    token_type: TokenType::Rbrace,
                    literal: "}".to_string(),
                },
                0 => Token {
                    token_type: TokenType::Eof,
                    literal: "".to_string(),
                },
                _ => Token {
                    token_type: TokenType::Illegal,
                    literal: "".to_string(),
                },
            };
            self.read_char();
            return next_token;
        }

        fn read_identifier(&mut self) -> Token {
            let start = self.position;

            while token::is_allowed_in_identifier(self.current_char) {
                self.read_char();
            }
            let identifier = std::str::from_utf8(&self.input[start..self.position])
                .unwrap()
                .to_string();

            // get the token type for this identifier
            let token_type = match identifier.as_str() {
                "fn" => TokenType::Function,
                "let" => TokenType::Let,
                "true" => TokenType::True,
                "false" => TokenType::False,
                "if" => TokenType::If,
                "else" => TokenType::Else,
                "return" => TokenType::Return,
                _ => TokenType::Identifier,
            };

            return Token {
                token_type: token_type,
                literal: identifier,
            };
        }

        fn read_number(&mut self) -> Token {
            let start = self.position;

            while token::is_allowed_in_int(self.current_char) {
                self.read_char();
            }
            let number = std::str::from_utf8(&self.input[start..self.position])
                .unwrap()
                .to_string();

            return Token {
                token_type: TokenType::Int,
                literal: number,
            };
        }

        fn read_char(&mut self) {
            if self.read_position >= self.input.len() {
                self.current_char = 0;
            } else {
                self.current_char = self.input[self.read_position];
            }
            self.position = self.read_position;
            self.read_position += 1;
        }

        fn peek_char(&mut self) -> u8 {
            if self.read_position >= self.input.len() {
                return 0;
            } else {
                return self.input[self.read_position];
            }
        }

        fn skip_whitespace(&mut self) {
            while self.current_char.is_ascii_whitespace() {
                self.read_char();
            }
        }
    }

    #[cfg(test)]
    mod tests {
        use crate::lexer::{
            token::{Token, TokenType},
            Lexer,
        };
        #[test]
        fn test_lexer() {
            let input = "let five = 5;
let ten = 10;

let add = fn(x, y) {
	x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
}

10 == 10;
10 != 9;
";
            let expected_tokens = vec![
                Token {
                    token_type: TokenType::Let,
                    literal: "let".to_string(),
                },
                Token {
                    token_type: TokenType::Identifier,
                    literal: "five".to_string(),
                },
                Token {
                    token_type: TokenType::Assign,
                    literal: "=".to_string(),
                },
                Token {
                    token_type: TokenType::Int,
                    literal: "5".to_string(),
                },
                Token {
                    token_type: TokenType::Semicolon,
                    literal: ";".to_string(),
                },
                Token {
                    token_type: TokenType::Let,
                    literal: "let".to_string(),
                },
                Token {
                    token_type: TokenType::Identifier,
                    literal: "ten".to_string(),
                },
                Token {
                    token_type: TokenType::Assign,
                    literal: "=".to_string(),
                },
                Token {
                    token_type: TokenType::Int,
                    literal: "10".to_string(),
                },
                Token {
                    token_type: TokenType::Semicolon,
                    literal: ";".to_string(),
                },
                Token {
                    token_type: TokenType::Let,
                    literal: "let".to_string(),
                },
                Token {
                    token_type: TokenType::Identifier,
                    literal: "add".to_string(),
                },
                Token {
                    token_type: TokenType::Assign,
                    literal: "=".to_string(),
                },
                Token {
                    token_type: TokenType::Function,
                    literal: "fn".to_string(),
                },
                Token {
                    token_type: TokenType::Lparen,
                    literal: "(".to_string(),
                },
                Token {
                    token_type: TokenType::Identifier,
                    literal: "x".to_string(),
                },
                Token {
                    token_type: TokenType::Comma,
                    literal: ",".to_string(),
                },
                Token {
                    token_type: TokenType::Identifier,
                    literal: "y".to_string(),
                },
                Token {
                    token_type: TokenType::Rparen,
                    literal: ")".to_string(),
                },
                Token {
                    token_type: TokenType::Lbrace,
                    literal: "{".to_string(),
                },
                Token {
                    token_type: TokenType::Identifier,
                    literal: "x".to_string(),
                },
                Token {
                    token_type: TokenType::Plus,
                    literal: "+".to_string(),
                },
                Token {
                    token_type: TokenType::Identifier,
                    literal: "y".to_string(),
                },
                Token {
                    token_type: TokenType::Semicolon,
                    literal: ";".to_string(),
                },
                Token {
                    token_type: TokenType::Rbrace,
                    literal: "}".to_string(),
                },
                Token {
                    token_type: TokenType::Semicolon,
                    literal: ";".to_string(),
                },
                Token {
                    token_type: TokenType::Let,
                    literal: "let".to_string(),
                },
                Token {
                    token_type: TokenType::Identifier,
                    literal: "result".to_string(),
                },
                Token {
                    token_type: TokenType::Assign,
                    literal: "=".to_string(),
                },
                Token {
                    token_type: TokenType::Identifier,
                    literal: "add".to_string(),
                },
                Token {
                    token_type: TokenType::Lparen,
                    literal: "(".to_string(),
                },
                Token {
                    token_type: TokenType::Identifier,
                    literal: "five".to_string(),
                },
                Token {
                    token_type: TokenType::Comma,
                    literal: ",".to_string(),
                },
                Token {
                    token_type: TokenType::Identifier,
                    literal: "ten".to_string(),
                },
                Token {
                    token_type: TokenType::Rparen,
                    literal: ")".to_string(),
                },
                Token {
                    token_type: TokenType::Semicolon,
                    literal: ";".to_string(),
                },
                Token {
                    token_type: TokenType::Bang,
                    literal: "!".to_string(),
                },
                Token {
                    token_type: TokenType::Minus,
                    literal: "-".to_string(),
                },
                Token {
                    token_type: TokenType::Slash,
                    literal: "/".to_string(),
                },
                Token {
                    token_type: TokenType::Asterisk,
                    literal: "*".to_string(),
                },
                Token {
                    token_type: TokenType::Int,
                    literal: "5".to_string(),
                },
                Token {
                    token_type: TokenType::Semicolon,
                    literal: ";".to_string(),
                },
                Token {
                    token_type: TokenType::Int,
                    literal: "5".to_string(),
                },
                Token {
                    token_type: TokenType::LessThan,
                    literal: "<".to_string(),
                },
                Token {
                    token_type: TokenType::Int,
                    literal: "10".to_string(),
                },
                Token {
                    token_type: TokenType::GreaterThan,
                    literal: ">".to_string(),
                },
                Token {
                    token_type: TokenType::Int,
                    literal: "5".to_string(),
                },
                Token {
                    token_type: TokenType::Semicolon,
                    literal: ";".to_string(),
                },
                Token {
                    token_type: TokenType::If,
                    literal: "if".to_string(),
                },
                Token {
                    token_type: TokenType::Lparen,
                    literal: "(".to_string(),
                },
                Token {
                    token_type: TokenType::Int,
                    literal: "5".to_string(),
                },
                Token {
                    token_type: TokenType::LessThan,
                    literal: "<".to_string(),
                },
                Token {
                    token_type: TokenType::Int,
                    literal: "10".to_string(),
                },
                Token {
                    token_type: TokenType::Rparen,
                    literal: ")".to_string(),
                },
                Token {
                    token_type: TokenType::Lbrace,
                    literal: "{".to_string(),
                },
                Token {
                    token_type: TokenType::Return,
                    literal: "return".to_string(),
                },
                Token {
                    token_type: TokenType::True,
                    literal: "true".to_string(),
                },
                Token {
                    token_type: TokenType::Semicolon,
                    literal: ";".to_string(),
                },
                Token {
                    token_type: TokenType::Rbrace,
                    literal: "}".to_string(),
                },
                Token {
                    token_type: TokenType::Else,
                    literal: "else".to_string(),
                },
                Token {
                    token_type: TokenType::Lbrace,
                    literal: "{".to_string(),
                },
                Token {
                    token_type: TokenType::Return,
                    literal: "return".to_string(),
                },
                Token {
                    token_type: TokenType::False,
                    literal: "false".to_string(),
                },
                Token {
                    token_type: TokenType::Semicolon,
                    literal: ";".to_string(),
                },
                Token {
                    token_type: TokenType::Rbrace,
                    literal: "}".to_string(),
                },
                Token {
                    token_type: TokenType::Int,
                    literal: "10".to_string(),
                },
                Token {
                    token_type: TokenType::Equal,
                    literal: "==".to_string(),
                },
                Token {
                    token_type: TokenType::Int,
                    literal: "10".to_string(),
                },
                Token {
                    token_type: TokenType::Semicolon,
                    literal: ";".to_string(),
                },
                Token {
                    token_type: TokenType::Int,
                    literal: "10".to_string(),
                },
                Token {
                    token_type: TokenType::NotEqual,
                    literal: "!=".to_string(),
                },
                Token {
                    token_type: TokenType::Int,
                    literal: "9".to_string(),
                },
                Token {
                    token_type: TokenType::Semicolon,
                    literal: ";".to_string(),
                },
                Token {
                    token_type: TokenType::Eof,
                    literal: "".to_string(),
                },
            ];

            let mut lexer = Lexer::new(input);

            for expected_token in expected_tokens.iter() {
                let token = lexer.next_token();

                assert_eq!(token.token_type, expected_token.token_type);
                assert_eq!(token.literal, expected_token.literal);
            }
        }
    }
}
