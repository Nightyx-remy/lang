use std::process::exit;
use crate::lexer::Lexer;
use crate::parser::{Parser};

mod token;
mod position;
mod lexer;
mod node;
mod parser;

fn main() {
    let str = std::fs::read_to_string("main.qt").unwrap();

    let mut lexer = Lexer::new(str);
    match lexer.tokenize() {
        Ok(tokens) => {
            for token in tokens.iter() {
                println!("{}", token);
            }
            println!();

            let str = lexer.free();

            let mut parser = Parser::new(tokens);
            match parser.parse() {
                Ok(ast) => {
                    for node in ast.iter() {
                        println!("{}", node);
                    }
                }
                Err(err) => {
                    println!("{}", err.error(str.clone()));
                    exit(-1);
                }
            }
        }
        Err(err) => {
            println!("{}", err.error(lexer.src.clone()));
            exit(-1);
        }
    }
}