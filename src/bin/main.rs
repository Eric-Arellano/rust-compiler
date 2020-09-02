use compiler::compiler;

fn main() {
    let result = compiler::execute_program(b"a, b; { a = 5 + b; print a; print b; }");
    match result {
        None => {
            eprintln!("Program did not run.");
            std::process::exit(0);
        }
        Some(res) => print!("{}", res),
    }
}
