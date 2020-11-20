/// The lexer converts the input program into discrete tokens, which may then be combined by the
/// parser to create an AST.

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    EndOfFile,
    Var,
    For,
    If,
    While,
    Switch,
    Case,
    Default,
    Print,
    Plus,
    Minus,
    Div,
    Mult,
    Equal,
    Colon,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    NotEqual,
    Greater,
    Less,
    Num(i64),
    Id(String),
}

pub struct Tokenizer {
    /// A buffer which tracks which `char`s have already been consumed from the input program.
    input_buffer: InputBuffer,
    /// A stack of tokens that have already been parsed, but not yet consumed. This allows the
    /// parser to "look ahead" and then "unget" those tokens if they don't end up being consumed.
    unconsumed_tokens: Vec<Token>,
}

impl Tokenizer {
    /// Create a lexer that will tokenize the input program.
    pub fn new(input: &[u8]) -> Tokenizer {
        Tokenizer {
            input_buffer: InputBuffer::new(input),
            unconsumed_tokens: Vec::with_capacity(2),
        }
    }

    /// Get the next token in the program.
    ///
    /// # Panics:
    ///
    /// `next` will panic if it encounters invalid tokens.
    #[track_caller]
    pub fn next(&mut self) -> Token {
        if let Some(token) = self.unconsumed_tokens.pop() {
            return token;
        }

        self.skip_space();
        match self.input_buffer.next() {
            None => Token::EndOfFile,
            Some(c) => match c {
                '+' => Token::Plus,
                '-' => Token::Minus,
                '/' => Token::Div,
                '*' => Token::Mult,
                '=' => Token::Equal,
                ':' => Token::Colon,
                ',' => Token::Comma,
                ';' => Token::Semicolon,
                '(' => Token::LParen,
                ')' => Token::RParen,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                '>' => Token::Greater,
                '<' => match self.input_buffer.peek() {
                    Some('>') => {
                        self.input_buffer.next();
                        Token::NotEqual
                    }
                    Some(_) => Token::Less,
                    None => Token::Less,
                },
                _ => {
                    if c.is_ascii_digit() {
                        self.parse_num(c)
                    } else if c.is_ascii_alphabetic() {
                        self.parse_id_or_keyword(c)
                    } else {
                        panic!("Unrecognized character: {}", c)
                    }
                }
            },
        }
    }

    pub fn peek(&mut self) -> Token {
        let tok = self.next();
        self.unconsumed_tokens.push(tok.clone());
        tok
    }

    pub fn peek_at_second(&mut self) -> Token {
        let t1 = self.next();
        let t2 = self.next();
        self.unconsumed_tokens.push(t2.clone());
        self.unconsumed_tokens.push(t1);
        t2
    }

    fn skip_space(&mut self) {
        while let Some(c) = self.input_buffer.peek() {
            if c.is_ascii_whitespace() {
                self.input_buffer.next();
                continue;
            } else {
                break;
            }
        }
    }

    #[track_caller]
    fn parse_num(&mut self, first_char: char) -> Token {
        let mut s = String::new();
        s.push(first_char);
        while let Some(c) = self.input_buffer.peek() {
            if c.is_ascii_digit() {
                self.input_buffer.next();
                s.push(c);
            } else {
                break;
            }
        }
        let num = s
            .parse()
            .unwrap_or_else(|_| panic!("Failed to parse as an integer: {}", s));
        Token::Num(num)
    }

    fn parse_id_or_keyword(&mut self, first_char: char) -> Token {
        let mut s = String::new();
        s.push(first_char);
        while let Some(c) = self.input_buffer.peek() {
            if c.is_ascii_alphanumeric() {
                self.input_buffer.next();
                s.push(c);
            } else {
                break;
            }
        }
        match s.as_str() {
            "VAR" => Token::Var,
            "FOR" => Token::For,
            "IF" => Token::If,
            "WHILE" => Token::While,
            "SWITCH" => Token::Switch,
            "CASE" => Token::Case,
            "DEFAULT" => Token::Default,
            "print" => Token::Print,
            _ => Token::Id(s),
        }
    }
}

struct InputBuffer {
    buffer: Vec<char>,
}

impl InputBuffer {
    fn new(input: &[u8]) -> InputBuffer {
        // Store in reverse order so that we can call `pop()` and `push()` to quickly access the
        // beginning of the program.
        InputBuffer {
            buffer: input.iter().map(|b| *b as char).rev().collect(),
        }
    }

    fn peek(&mut self) -> Option<char> {
        let c = self.buffer.pop();
        if let Some(c) = c {
            self.buffer.push(c);
        }
        c
    }

    fn next(&mut self) -> Option<char> {
        self.buffer.pop()
    }
}

#[test]
fn test_input_buffer() {
    let mut buffer = InputBuffer::new(b"abc");
    assert_eq!(buffer.peek(), Some('a'));
    assert_eq!(buffer.peek(), Some('a'));
    assert_eq!(buffer.next(), Some('a'));
    assert_eq!(buffer.peek(), Some('b'));
    assert_eq!(buffer.next(), Some('b'));
    assert_eq!(buffer.peek(), Some('c'));
    assert_eq!(buffer.next(), Some('c'));
    assert_eq!(buffer.peek(), None);
    assert_eq!(buffer.next(), None);
}

#[test]
fn test_get_token() {
    // Check parsing the end of the file.
    let mut lexer = Tokenizer::new(b"");
    assert_eq!(lexer.next(), Token::EndOfFile);
    assert_eq!(lexer.next(), Token::EndOfFile);

    fn assert_single_token_parsed(input: &[u8], expected: Token) {
        let mut lexer = Tokenizer::new(input);
        assert_eq!(lexer.next(), expected);
        // Ensure that the Lexer properly "consumes" the expected token.
        assert_eq!(lexer.next(), Token::EndOfFile);
    }

    // Check that single chars can be parsed properly on their own.
    assert_single_token_parsed(b"+", Token::Plus);
    assert_single_token_parsed(b"-", Token::Minus);
    assert_single_token_parsed(b"/", Token::Div);
    assert_single_token_parsed(b"*", Token::Mult);
    assert_single_token_parsed(b"=", Token::Equal);
    assert_single_token_parsed(b":", Token::Colon);
    assert_single_token_parsed(b",", Token::Comma);
    assert_single_token_parsed(b";", Token::Semicolon);
    assert_single_token_parsed(b"(", Token::LParen);
    assert_single_token_parsed(b")", Token::RParen);
    assert_single_token_parsed(b"{", Token::LBrace);
    assert_single_token_parsed(b"}", Token::RBrace);
    assert_single_token_parsed(b">", Token::Greater);

    // Check for `<` vs. `<>`.
    assert_single_token_parsed(b"<", Token::Less);
    assert_single_token_parsed(b"<>", Token::NotEqual);
    // Because we "look ahead", we need to make sure this does not break parsing of subsequent
    // tokens.
    let mut lexer = Tokenizer::new(b"<+");
    assert_eq!(lexer.next(), Token::Less);
    assert_eq!(lexer.next(), Token::Plus);
    assert_eq!(lexer.next(), Token::EndOfFile);

    // Check parsing of integers.
    assert_single_token_parsed(b"0", Token::Num(0));
    assert_single_token_parsed(b"8", Token::Num(8));
    assert_single_token_parsed(b"8131", Token::Num(8131));
    assert_single_token_parsed(b"0002", Token::Num(2));
    let mut lexer = Tokenizer::new(b"12+24");
    assert_eq!(lexer.next(), Token::Num(12));
    assert_eq!(lexer.next(), Token::Plus);
    assert_eq!(lexer.next(), Token::Num(24));
    assert_eq!(lexer.next(), Token::EndOfFile);

    // Check parsing of keywords.
    assert_single_token_parsed(b"VAR", Token::Var);
    assert_single_token_parsed(b"FOR", Token::For);
    assert_single_token_parsed(b"IF", Token::If);
    assert_single_token_parsed(b"WHILE", Token::While);
    assert_single_token_parsed(b"SWITCH", Token::Switch);
    assert_single_token_parsed(b"CASE", Token::Case);
    assert_single_token_parsed(b"DEFAULT", Token::Default);
    assert_single_token_parsed(b"print", Token::Print);

    // Check parsing of Ids.
    assert_single_token_parsed(b"x", Token::Id("x".to_string()));
    assert_single_token_parsed(b"FooBar21", Token::Id("FooBar21".to_string()));
    assert_single_token_parsed(b"vAR", Token::Id("vAR".to_string()));
    let mut lexer = Tokenizer::new(b"x,y z");
    assert_eq!(lexer.next(), Token::Id("x".to_string()));
    assert_eq!(lexer.next(), Token::Comma);
    assert_eq!(lexer.next(), Token::Id("y".to_string()));
    assert_eq!(lexer.next(), Token::Id("z".to_string()));
    assert_eq!(lexer.next(), Token::EndOfFile);

    // Check that it all works together, including that we handle spaces properly.
    let mut lexer = Tokenizer::new(
        b"VAR x, y;
        { IF x <> y {
           print x;
           print y;
           }
        }",
    );
    let x = Token::Id("x".to_string());
    let y = Token::Id("y".to_string());
    assert_eq!(lexer.next(), Token::Var);
    assert_eq!(lexer.next(), x);
    assert_eq!(lexer.next(), Token::Comma);
    assert_eq!(lexer.next(), y);
    assert_eq!(lexer.next(), Token::Semicolon);
    assert_eq!(lexer.next(), Token::LBrace);
    assert_eq!(lexer.next(), Token::If);
    assert_eq!(lexer.next(), x);
    assert_eq!(lexer.next(), Token::NotEqual);
    assert_eq!(lexer.next(), y);
    assert_eq!(lexer.next(), Token::LBrace);
    assert_eq!(lexer.next(), Token::Print);
    assert_eq!(lexer.next(), x);
    assert_eq!(lexer.next(), Token::Semicolon);
    assert_eq!(lexer.next(), Token::Print);
    assert_eq!(lexer.next(), y);
    assert_eq!(lexer.next(), Token::Semicolon);
    assert_eq!(lexer.next(), Token::RBrace);
    assert_eq!(lexer.next(), Token::RBrace);
    assert_eq!(lexer.next(), Token::EndOfFile);
}

#[should_panic(expected = "Unrecognized character: !")]
#[test]
fn test_get_token_invalid() {
    Tokenizer::new(b"!").next();
}

#[test]
fn test_tokenizer_peek() {
    let mut lexer = Tokenizer::new(b":(");
    assert_eq!(lexer.peek(), Token::Colon);
    assert_eq!(lexer.peek_at_second(), Token::LParen);
}
