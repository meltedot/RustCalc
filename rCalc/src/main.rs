#![allow(dead_code)]
mod lexer;
use std::io;
use std::io::*;

fn runlexer(input: &str) {
    match lexer::lex(input) {
        Ok(v) => println!("{:?}", v),
        Err(e) => println!("{}", e.panic())
    }
}


fn main() {
    let mut parser = lexer::Parser::new();
    loop {
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Failed to read from stdin");
        if input == "exit" {
            break;
        }
        lexer::parse(lexer::lex(input.as_str()), &mut parser);
    }
}
