use std::env;
use std::io::Write;
use crate::lexer::Lexer;
use crate::token::TokenType;

mod token;
mod lexer;

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
        while let token = lexer.next_token() {
            if token.token_type == TokenType::Eof {
                break;
            }
            println!("{:#?}", token);
        }
    }
}
