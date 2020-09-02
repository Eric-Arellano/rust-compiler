extern crate compiler;

use std::fs::File;
use std::io::{BufReader, Read};
use std::path::Path;

use compiler::compiler::execute_program;

#[cfg(test)]
fn assert_example(file_name: &str, expected: &[i64]) {
    let f = File::open(Path::new(format!("examples/{}", file_name).as_str())).unwrap();
    let mut s = String::new();
    BufReader::new(f).read_to_string(&mut s).unwrap();
    let result = execute_program(s.as_bytes());
    let expected: Vec<String> = expected.iter().map(|i| format!("{}", i)).collect();
    assert_eq!(result, Some(expected.join("\n") + "\n"));
}

#[test]
fn test_print() {
    assert_example("print1.txt", &[0]);
    assert_example("print2.txt", &[0, 0]);
    assert_example("print3.txt", &[0, 0, 0, 0]);
}

#[test]
fn test_assignment() {
    assert_example("assignment1.txt", &[42]);
    assert_example("assignment2.txt", &[457, 221, 65537, 12481632]);
    assert_example("assignment3.txt", &[0, 0]);
    assert_example("assignment4.txt", &[6, 4, 7, 3]);
    assert_example(
        "assignment5.txt",
        &[6, 2, 6, 4, 1, 4, 4, 4, 1, 4, 3, 3, -1, 0, 4, 3, -1],
    );
}
