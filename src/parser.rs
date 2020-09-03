/// The parser converts `lexer::Token`s into an AST according to the grammar in README.md.
use crate::lexer::{Lexer, Token};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Print(String),
    Assign(String, Assignment),
    If {
        condition: Condition,
        true_branch: Option<Box<StatementNode>>,
        false_branch: Option<Box<StatementNode>>,
    },
    Goto(Box<StatementNode>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct StatementNode {
    pub statement: Statement,
    pub next: Option<Box<StatementNode>>,
}

impl StatementNode {
    fn append(&mut self, v: StatementNode) {
        match &mut self.next {
            Some(ref mut x) => x.append(v),
            None => self.next = Some(Box::new(v)),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Primary {
    Id(String),
    Num(i64),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum ArithmeticOperator {
    Plus,
    Minus,
    Mult,
    Div,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Assignment {
    Primary(Primary),
    Expr(Primary, ArithmeticOperator, Primary),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum RelativeOperator {
    Less,
    Greater,
    NotEqual,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Condition {
    pub operand1: Primary,
    pub operand2: Primary,
    pub op: RelativeOperator,
}

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(input: &[u8]) -> Parser {
        Parser {
            lexer: Lexer::new(input),
        }
    }

    pub fn parse_program(&mut self) -> (Vec<String>, Option<StatementNode>) {
        let var_names = self.parse_var_declaration();
        let first_statement = self.parse_body();
        self.expect(Token::EndOfFile);
        (var_names, first_statement)
    }

    /// Consume the specified token, or panic if it is not the next token.
    ///
    /// This function will discard the specified token, so it is not useful for tokens with data
    /// (like `Id` and `Num`).
    #[track_caller]
    fn expect(&mut self, token: Token) {
        let parsed = self.lexer.get_token();
        if parsed != token {
            panic!("Unexpected token: {:?}, expected: {:?}", parsed, token);
        }
    }

    /// Consume an `Id` token and return its `String`, or panic if the next token is not an `Id`.
    #[track_caller]
    fn expect_id(&mut self) -> String {
        match self.lexer.get_token() {
            Token::Id(n) => n,
            tok => panic!("Unexpected token: {:?}, expected: Id.", tok),
        }
    }

    // Consume a `Num` token and return its `i64`, or panic if the next token is not a `Num`.
    #[track_caller]
    fn expect_num(&mut self) -> i64 {
        match self.lexer.get_token() {
            Token::Num(i) => i,
            tok => panic!("Unexpected token: {:?}, expected: Num.", tok),
        }
    }

    // Consume a `Num` or `Id` token, or panic if the next token is not one of these two`.
    #[track_caller]
    fn expect_primary(&mut self) -> Primary {
        match self.lexer.get_token() {
            Token::Id(s) => Primary::Id(s),
            Token::Num(i) => Primary::Num(i),
            tok => panic!("Unexpected token: {:?}, expected: Num or Id.", tok),
        }
    }

    /// Peek at what the next token will be without actually consuming the value.
    fn peek(&mut self) -> Token {
        let t = self.lexer.get_token();
        self.lexer.unget_token(t.clone());
        t
    }

    /// Peek at what the second-upcoming token will be without actually consuming the value.
    fn peek_at_second(&mut self) -> Token {
        let t1 = self.lexer.get_token();
        let t2 = self.lexer.get_token();
        self.lexer.unget_token(t2.clone());
        self.lexer.unget_token(t1);
        t2
    }

    /// Get all variables to be used in the program.
    ///
    /// This only gets the variable names, as no initial value may be set. All variables are
    /// initialized to 0.
    #[track_caller]
    fn parse_var_declaration(&mut self) -> Vec<String> {
        fn parse_id_list<'a>(
            s: &mut Parser,
            parsed_ids: &'a mut Vec<String>,
        ) -> &'a mut Vec<String> {
            parsed_ids.push(s.expect_id());
            match s.peek() {
                Token::Comma => {
                    s.expect(Token::Comma);
                    parse_id_list(s, parsed_ids)
                }
                _ => parsed_ids,
            }
        }

        let mut ids = Vec::new();
        let parsed_ids = parse_id_list(self, &mut ids);
        self.expect(Token::Semicolon);
        parsed_ids.to_vec()
    }

    /// Parse the statements contained in brackets {}.
    fn parse_body(&mut self) -> Option<StatementNode> {
        self.expect(Token::LBrace);

        fn is_statement(tok1: &Token, tok2: &Token) -> bool {
            match (tok1, tok2) {
                (Token::Id(_), Token::Equal)
                | (Token::Print, _)
                | (Token::If, _)
                | (Token::While, _)
                | (Token::Switch, _) => true,
                _ => false,
            }
        }

        fn parse_statement_list(s: &mut Parser) -> Option<StatementNode> {
            let statement_node_opt = match (s.peek(), s.peek_at_second()) {
                (Token::Id(_), Token::Equal) => Some(s.parse_assignment()),
                (Token::Print, _) => Some(s.parse_print()),
                (Token::If, _) => Some(s.parse_if()),
                _ => None,
            };
            match statement_node_opt {
                None => None,
                Some(ref statement_node) => {
                    // Check if there are additional statements to parse.
                    if is_statement(&s.peek(), &s.peek_at_second()) {
                        let next_statement_node = parse_statement_list(s).map(Box::new);
                        match &statement_node.statement {
                            Statement::If {
                                condition,
                                true_branch,
                                false_branch,
                            } => {
                                // If it's an `If` statement, and the next node exists, we must
                                // append the next node to `true_branch` and `false_branch`. The
                                // `next` value is kept as `None`, as it gets ignored in the
                                // compiler.
                                // TODO: fix support for nested ifs.
                                // TODO: refactor this.
                                if let Some(ref next_statement) = next_statement_node {
                                    let updated_true_branch = if let Some(true_stmt) = true_branch {
                                        let mut true_stmt = true_stmt.clone();
                                        true_stmt.append(*next_statement.clone());
                                        Some(true_stmt)
                                    } else {
                                        next_statement_node.clone()
                                    };
                                    let updated_false_branch =
                                        if let Some(false_stmt) = false_branch {
                                            let mut false_stmt = false_stmt.clone();
                                            false_stmt.append(*next_statement.clone());
                                            Some(false_stmt)
                                        } else {
                                            next_statement_node
                                        };
                                    let updated_statement = Statement::If {
                                        condition: condition.clone(),
                                        true_branch: updated_true_branch,
                                        false_branch: updated_false_branch,
                                    };
                                    Some(StatementNode {
                                        statement: updated_statement,
                                        next: None,
                                    })
                                } else {
                                    Some(statement_node.clone())
                                }
                            }
                            _ => {
                                // Otherwise, point the statement's `next` value to the next
                                // node.
                                Some(StatementNode {
                                    statement: statement_node.statement.clone(),
                                    next: next_statement_node,
                                })
                            }
                        }
                    } else {
                        statement_node_opt
                    }
                }
            }
        }

        let statement_opt = parse_statement_list(self);
        self.expect(Token::RBrace);
        statement_opt
    }

    fn parse_assignment(&mut self) -> StatementNode {
        let id = self.expect_id();
        self.expect(Token::Equal);

        let is_expression = match self.peek() {
            Token::Id(_) | Token::Num(_) => match self.peek_at_second() {
                Token::Plus | Token::Minus | Token::Mult | Token::Div => true,
                _ => false,
            },
            _ => false,
        };

        let assignment = if is_expression {
            let primary1 = self.expect_primary();
            let op = match self.lexer.get_token() {
                Token::Plus => ArithmeticOperator::Plus,
                Token::Minus => ArithmeticOperator::Minus,
                Token::Mult => ArithmeticOperator::Mult,
                Token::Div => ArithmeticOperator::Div,
                tok => panic!(
                    "Unexpected token: {:?}, expected: Plus, Minus, Mult, or Div.",
                    tok
                ),
            };
            let primary2 = self.expect_primary();
            Assignment::Expr(primary1, op, primary2)
        } else {
            Assignment::Primary(self.expect_primary())
        };

        self.expect(Token::Semicolon);
        StatementNode {
            statement: Statement::Assign(id, assignment),
            next: None,
        }
    }

    fn parse_print(&mut self) -> StatementNode {
        self.expect(Token::Print);
        let id = self.expect_id();
        self.expect(Token::Semicolon);
        StatementNode {
            statement: Statement::Print(id),
            next: None,
        }
    }

    fn parse_if(&mut self) -> StatementNode {
        self.expect(Token::If);
        let condition = self.parse_condition();
        let if_body = self.parse_body();
        StatementNode {
            statement: Statement::If {
                condition,
                true_branch: if_body.map(Box::new),
                false_branch: None,
            },
            next: None,
        }
    }

    fn parse_condition(&mut self) -> Condition {
        let operand1 = self.expect_primary();
        let op = match self.lexer.get_token() {
            Token::Less => RelativeOperator::Less,
            Token::Greater => RelativeOperator::Greater,
            Token::NotEqual => RelativeOperator::NotEqual,
            tok => panic!(
                "Unexpected token: {:?}, expected: Less, Greater, or NotEqual.",
                tok
            ),
        };
        let operand2 = self.expect_primary();
        Condition {
            operand1,
            operand2,
            op,
        }
    }
}

#[test]
fn test_parse_print() {
    assert_eq!(
        Parser::new(b"print a;").parse_print(),
        StatementNode {
            statement: Statement::Print("a".to_string()),
            next: None
        }
    );
}

#[test]
fn test_parse_assignment() {
    assert_eq!(
        Parser::new(b"a = 10;").parse_assignment(),
        StatementNode {
            statement: Statement::Assign("a".to_string(), Assignment::Primary(Primary::Num(10))),
            next: None
        }
    );
    assert_eq!(
        Parser::new(b"x = y / 4;").parse_assignment(),
        StatementNode {
            statement: Statement::Assign(
                "x".to_string(),
                Assignment::Expr(
                    Primary::Id("y".to_string()),
                    ArithmeticOperator::Div,
                    Primary::Num(4)
                )
            ),
            next: None
        }
    );
}

#[test]
fn test_parse_if() {
    assert_eq!(
        Parser::new(b"IF a <> 2 { print a; }").parse_if(),
        StatementNode {
            statement: Statement::If {
                true_branch: Some(Box::new(StatementNode {
                    statement: Statement::Print("a".to_string()),
                    next: None,
                })),
                false_branch: None,
                condition: Condition {
                    operand1: Primary::Id("a".to_string()),
                    operand2: Primary::Num(2),
                    op: RelativeOperator::NotEqual
                }
            },
            next: None
        }
    );
}
