use crate::lexer::Lexer;
use crate::token::{Token, TokenType};
use std::env;
use std::io::Write;

mod lexer;
mod token;

const PROMPT: &str = ">>";

fn main() {
    let user = env::var("USER").unwrap();
    println!("Welcome {user}!");
    println!("This is the Monkey programming language REPL.\n");

    loop {
        print!("{PROMPT} ");
        std::io::stdout().flush().unwrap();

        let mut input_line = String::new();
        std::io::stdin().read_line(&mut input_line).unwrap();

        let mut lexer = Lexer::new(input_line.as_str());
        let mut token = Token {
            token_type: TokenType::Illegal,
            literal: "".to_string(),
        };
        while token.token_type != TokenType::Eof {
            token = lexer.next_token();
            println!("{:#?}", token);
        }
    }
}
