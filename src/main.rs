use crate::lexer::Lexer;
use crate::parser::Parser;
use std::env;
use std::io::Write;

mod ast;
mod lexer;
mod parser;
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

        let lexer = Lexer::new(input_line.as_str());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        if parser.errors.len() > 0 {
            for error in parser.errors.iter() {
                println!("Error: {error}");
            }
        } else {
            println!("{}", program);
        }
    }
}
