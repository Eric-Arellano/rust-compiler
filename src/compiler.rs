/// The compiler executes the intermediate representation graph created by the Parser.
use std::collections::HashMap;

use crate::parser::{ArithmeticOperator, Assignment, Parser, Primary, Statement, StatementNode};

struct ProgramCounter {
    current: Option<Box<StatementNode>>,
}

impl ProgramCounter {
    fn new(first_statement: StatementNode) -> ProgramCounter {
        ProgramCounter {
            current: Some(Box::new(first_statement)),
        }
    }

    fn advance(&mut self) {
        if let Some(ref current_stmt_node) = self.current {
            self.current = current_stmt_node.next.clone();
        }
    }
}

struct Variables {
    ids_to_vals: HashMap<String, i64>,
}

impl Variables {
    fn new(ids: Vec<String>) -> Variables {
        let mut map = HashMap::new();
        for id in ids {
            map.insert(id, 0);
        }
        Variables { ids_to_vals: map }
    }

    fn get(&self, id: &str) -> i64 {
        let v = self
            .ids_to_vals
            .get(id)
            .unwrap_or_else(|| panic!("Reference to unknown variable {}", id));
        *v
    }

    fn set(&mut self, id: &str, v: i64) {
        let res = self.ids_to_vals.insert(id.to_string(), v);
        if res.is_none() {
            panic!("Reference to unknown variable {}", id)
        }
    }
}

pub fn execute_program(program: &[u8]) -> Option<String> {
    let (var_names, first_statement) = Parser::new(program).parse_program();
    first_statement.as_ref()?;
    let mut variables = Variables::new(var_names);
    let mut program_counter = ProgramCounter::new(first_statement.unwrap());

    fn resolve_primary(vars: &Variables, primary: &Primary) -> i64 {
        match primary {
            Primary::Num(i) => *i,
            Primary::Id(s) => vars.get(&s),
        }
    }

    let mut result_str = String::new();
    loop {
        match &program_counter.current {
            None => break,
            Some(current) => {
                match &current.statement {
                    Statement::Noop => (),
                    Statement::Print(id) => {
                        result_str.push_str(format!("{}\n", variables.get(id)).as_str())
                    }
                    Statement::Assign(id, assignment) => {
                        let result = match &assignment {
                            Assignment::Primary(v) => resolve_primary(&variables, v),
                            Assignment::Expr(v1, op, v2) => {
                                let v1 = resolve_primary(&variables, v1);
                                let v2 = resolve_primary(&variables, v2);
                                match op {
                                    ArithmeticOperator::Plus => v1 + v2,
                                    ArithmeticOperator::Minus => v1 - v2,
                                    ArithmeticOperator::Mult => v1 * v2,
                                    ArithmeticOperator::Div => v1 / v2,
                                }
                            }
                        };
                        variables.set(id, result);
                    }
                    _ => (),
                }
                program_counter.advance();
            }
        }
    }
    Some(result_str)
}
