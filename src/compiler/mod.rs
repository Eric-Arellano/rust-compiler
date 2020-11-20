/// The compiler executes the intermediate representation graph created by the Parser.
use std::collections::HashMap;

use crate::parser::{ArithmeticOperator, Assignment, Parser, Primary, RelativeOperator, Statement};

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

    fn resolve_primary(vars: &Variables, primary: &Primary) -> i64 {
        match primary {
            Primary::Num(i) => *i,
            Primary::Id(s) => vars.get(&s),
        }
    }

    let mut current_opt = Some(Box::new(first_statement.unwrap()));
    let mut result_str = String::new();
    loop {
        match &current_opt {
            None => break,
            Some(current) => {
                let advance_to_opt = match &current.statement {
                    Statement::Print(id) => {
                        result_str.push_str(format!("{}\n", variables.get(id)).as_str());
                        None
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
                        None
                    }
                    Statement::If {
                        condition,
                        true_branch,
                        false_branch,
                    } => {
                        let op1 = resolve_primary(&variables, &condition.operand1);
                        let op2 = resolve_primary(&variables, &condition.operand2);
                        let is_true = match condition.op {
                            RelativeOperator::Less => op1 < op2,
                            RelativeOperator::Greater => op1 > op2,
                            RelativeOperator::NotEqual => op1 != op2,
                        };
                        let next = if is_true { true_branch } else { false_branch };
                        Some(next)
                    }
                    _ => None,
                };
                if let Some(advance_to) = advance_to_opt {
                    current_opt = advance_to.clone();
                } else {
                    current_opt = current.next.clone();
                }
            }
        }
    }
    Some(result_str)
}
