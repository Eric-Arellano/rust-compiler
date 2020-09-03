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
    let ending = if expected.len() > 0 { "\n" } else { "" };
    let expected = Some(expected.join("\n") + ending);
    assert_eq!(result, expected, "testing examples/{}", file_name);
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

#[test]
fn test_if() {
    assert_example("if1_a.txt", &[]);
    assert_example("if1_b.txt", &[1]);
    assert_example("if2_a.txt", &[]);
    assert_example("if2_b.txt", &[2, 1, 3, 5]);
    assert_example("if3_a.txt", &[3]);
    assert_example("if3_b.txt", &[2, 1, 3, 5, 8]);
    assert_example("if4_a.txt", &[3, 2, 3, 5, 7]);
    assert_example("if4_b.txt", &[2, 1, 3, 5, 8, 5, 8, 3, 8]);
    assert_example("if5_a.txt", &[2, 1, 3, 5, 8, 5, 8, 3, 8, 24]);
    assert_example("if5_b.txt", &[3, 6]);
    assert_example("if6_a.txt", &[1]);
    assert_example("if6_b.txt", &[]);
    // assert_example("if7.txt", &[3, 3, 5, 10, 2]);
    // assert_example("if8.txt", &[3, 3, 3, 5, 8, 40, 5]);
    // assert_example("if9.txt", &[3, 3, 3, 5, 8, 40, 5, 40, 41, 1]);
    // assert_example(
    //     "if10.txt",
    //     &[0, 0, 9, 14, 1, 2, 9, 14, 3, 3, 3, 5, 8, 40, 5, 40, 41, 1],
    // );
    // assert_example(
    //     "if11.txt",
    //     &[0, 0, 9, 14, 2, 3, 1, 2, 9, 14, 3, 3, 3, 5, 8, 40, 5, 40, 41, 1],
    // );
    // assert_example(
    //     "if12.txt",
    //     &[0, 0, 9, 14, 0, 0, 2, 3, 3, 1, 2, 9, 14, 3, 3, 3, 5, 8, 40, 5, 40, 41, 1],
    // );
}
