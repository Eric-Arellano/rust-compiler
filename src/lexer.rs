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

#[derive(Debug, Clone, PartialEq)]
pub struct InvalidTokenError(String);

impl std::fmt::Display for InvalidTokenError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for InvalidTokenError {
    fn description(&self) -> &str {
        &self.0
    }
}

pub struct Lexer {
    /// A buffer which tracks which `char`s have already been consumed from the input program.
    input_buffer: InputBuffer,
    /// A stack of tokens that have already been parsed, but not yet consumed. This allows the
    /// parser to "look ahead" and then "unget" those tokens if they don't end up being consumed.
    unconsumed_tokens: Vec<Token>,
}

impl Lexer {
    /// Create a lexer that will tokenize the input program.
    pub fn new(input: &[u8]) -> Lexer {
        Lexer {
            input_buffer: InputBuffer::new(input),
            unconsumed_tokens: Vec::new(),
        }
    }

    /// Get the next token in the program.
    pub fn get_token(&mut self) -> Result<Token, InvalidTokenError> {
        if let Some(token) = self.unconsumed_tokens.pop() {
            return Ok(token);
        }

        self.skip_space();
        match self.input_buffer.get_char() {
            None => Ok(Token::EndOfFile),
            Some(c) => match c {
                '+' => Ok(Token::Plus),
                '-' => Ok(Token::Minus),
                '/' => Ok(Token::Div),
                '*' => Ok(Token::Mult),
                '=' => Ok(Token::Equal),
                ':' => Ok(Token::Colon),
                ',' => Ok(Token::Comma),
                ';' => Ok(Token::Semicolon),
                '(' => Ok(Token::LParen),
                ')' => Ok(Token::RParen),
                '{' => Ok(Token::LBrace),
                '}' => Ok(Token::RBrace),
                '>' => Ok(Token::Greater),
                '<' => match self.input_buffer.get_char() {
                    Some('>') => Ok(Token::NotEqual),
                    Some(c) => {
                        self.input_buffer.unget_char(c);
                        Ok(Token::Less)
                    }
                    None => Ok(Token::Less),
                },
                _ => {
                    if c.is_ascii_digit() {
                        self.parse_num(c)
                    } else if c.is_ascii_alphabetic() {
                        Ok(self.parse_id_or_keyword(c))
                    } else {
                        Err(InvalidTokenError(format!("Unrecognized character: {}", c)))
                    }
                }
            },
        }
    }

    /// Return a token back to the input so that it can be parsed again.
    ///
    /// This is useful to "look ahead" to certain tokens without having to consume them.
    pub fn unget_token(&mut self, token: Token) {
        self.unconsumed_tokens.push(token);
    }

    fn skip_space(&mut self) {
        while let Some(c) = self.input_buffer.get_char() {
            if c.is_ascii_whitespace() {
                continue;
            } else {
                self.input_buffer.unget_char(c);
                break;
            }
        }
    }

    fn parse_num(&mut self, first_char: char) -> Result<Token, InvalidTokenError> {
        let mut s = String::new();
        s.push(first_char);
        while let Some(c) = self.input_buffer.get_char() {
            if c.is_ascii_digit() {
                s.push(c);
            } else {
                self.input_buffer.unget_char(c);
                break;
            }
        }
        match s.parse() {
            Ok(n) => Ok(Token::Num(n)),
            Err(e) => Err(InvalidTokenError(format!("{}", e))),
        }
    }

    fn parse_id_or_keyword(&mut self, first_char: char) -> Token {
        let mut s = String::new();
        s.push(first_char);
        while let Some(c) = self.input_buffer.get_char() {
            if c.is_ascii_alphanumeric() {
                s.push(c);
            } else {
                self.input_buffer.unget_char(c);
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
    buffer: Vec<u8>,
}

impl InputBuffer {
    fn new(input: &[u8]) -> InputBuffer {
        let mut buffer = input.to_vec();
        // Store in reverse order so that we can call `pop()` and `push()` to quickly access the
        // beginning of the program.
        buffer.reverse();
        InputBuffer { buffer }
    }

    fn get_char(&mut self) -> Option<char> {
        self.buffer.pop().map(|v| v as char)
    }

    fn unget_char(&mut self, c: char) {
        self.buffer.push(c as u8);
    }
}

#[test]
fn test_input_buffer() {
    let mut buffer = InputBuffer::new(b"abc");
    assert_eq!(buffer.get_char(), Some('a'));
    assert_eq!(buffer.get_char(), Some('b'));
    buffer.unget_char('z');
    assert_eq!(buffer.get_char(), Some('z'));
    assert_eq!(buffer.get_char(), Some('c'));
    assert_eq!(buffer.get_char(), None);
}

#[test]
fn test_get_token() {
    // Check parsing the end of the file.
    let mut lexer = Lexer::new(b"");
    assert_eq!(lexer.get_token(), Ok(Token::EndOfFile));
    assert_eq!(lexer.get_token(), Ok(Token::EndOfFile));

    fn assert_single_token_parsed(input: &[u8], expected: Token) {
        let mut lexer = Lexer::new(input);
        assert_eq!(lexer.get_token(), Ok(expected));
        // Ensure that the Lexer properly "consumes" the expected token.
        assert_eq!(lexer.get_token(), Ok(Token::EndOfFile));
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
    let mut lexer = Lexer::new(b"<+");
    assert_eq!(lexer.get_token(), Ok(Token::Less));
    assert_eq!(lexer.get_token(), Ok(Token::Plus));
    assert_eq!(lexer.get_token(), Ok(Token::EndOfFile));

    // Check parsing of integers.
    assert_single_token_parsed(b"0", Token::Num(0));
    assert_single_token_parsed(b"8", Token::Num(8));
    assert_single_token_parsed(b"8131", Token::Num(8131));
    assert_single_token_parsed(b"0002", Token::Num(2));
    let mut lexer = Lexer::new(b"12+24");
    assert_eq!(lexer.get_token(), Ok(Token::Num(12)));
    assert_eq!(lexer.get_token(), Ok(Token::Plus));
    assert_eq!(lexer.get_token(), Ok(Token::Num(24)));
    assert_eq!(lexer.get_token(), Ok(Token::EndOfFile));

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
    let mut lexer = Lexer::new(b"x,y z");
    assert_eq!(lexer.get_token(), Ok(Token::Id("x".to_string())));
    assert_eq!(lexer.get_token(), Ok(Token::Comma));
    assert_eq!(lexer.get_token(), Ok(Token::Id("y".to_string())));
    assert_eq!(lexer.get_token(), Ok(Token::Id("z".to_string())));
    assert_eq!(lexer.get_token(), Ok(Token::EndOfFile));

    // Check invalid input. We should be able to gracefully parse the rest of the tokens.
    let mut lexer = Lexer::new(b"! underscore_banned");
    assert_eq!(
        lexer.get_token(),
        Err(InvalidTokenError("Unrecognized character: !".to_string()))
    );
    assert_eq!(lexer.get_token(), Ok(Token::Id("underscore".to_string())));
    assert_eq!(
        lexer.get_token(),
        Err(InvalidTokenError("Unrecognized character: _".to_string()))
    );
    assert_eq!(lexer.get_token(), Ok(Token::Id("banned".to_string())));
    assert_eq!(lexer.get_token(), Ok(Token::EndOfFile));

    // Check that it all works together, including that we handle spaces properly.
    let mut lexer = Lexer::new(
        b"VAR x, y;
        { IF x <> y {
           print x;
           print y;
           }
        }",
    );
    let x = Ok(Token::Id("x".to_string()));
    let y = Ok(Token::Id("y".to_string()));
    assert_eq!(lexer.get_token(), Ok(Token::Var));
    assert_eq!(lexer.get_token(), x);
    assert_eq!(lexer.get_token(), Ok(Token::Comma));
    assert_eq!(lexer.get_token(), y);
    assert_eq!(lexer.get_token(), Ok(Token::Semicolon));
    assert_eq!(lexer.get_token(), Ok(Token::LBrace));
    assert_eq!(lexer.get_token(), Ok(Token::If));
    assert_eq!(lexer.get_token(), x);
    assert_eq!(lexer.get_token(), Ok(Token::NotEqual));
    assert_eq!(lexer.get_token(), y);
    assert_eq!(lexer.get_token(), Ok(Token::LBrace));
    assert_eq!(lexer.get_token(), Ok(Token::Print));
    assert_eq!(lexer.get_token(), x);
    assert_eq!(lexer.get_token(), Ok(Token::Semicolon));
    assert_eq!(lexer.get_token(), Ok(Token::Print));
    assert_eq!(lexer.get_token(), y);
    assert_eq!(lexer.get_token(), Ok(Token::Semicolon));
    assert_eq!(lexer.get_token(), Ok(Token::RBrace));
    assert_eq!(lexer.get_token(), Ok(Token::RBrace));
    assert_eq!(lexer.get_token(), Ok(Token::EndOfFile));
}

#[test]
fn test_unget_token() {
    let mut lexer = Lexer::new(b"");
    lexer.unget_token(Token::Colon);
    lexer.unget_token(Token::LParen);
    assert_eq!(lexer.get_token(), Ok(Token::LParen));
    assert_eq!(lexer.get_token(), Ok(Token::Colon));
    assert_eq!(lexer.get_token(), Ok(Token::EndOfFile));
}