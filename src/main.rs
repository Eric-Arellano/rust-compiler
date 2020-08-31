mod compiler;
mod lexer;
mod parser;

use crate::parser::Parser;

fn main() {
    let mut parser = Parser::new(b"a, b; { a = 5 + b; print a; print b; }");
    parser.parse_program();
    println!("Parsed!");
}
