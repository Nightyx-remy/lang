use crate::position::{Position, PositionedObject};
use crate::token::{PositionedToken, Token, Keyword};
use std::fmt::{Display, Formatter};

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                           Lexer Error                                          //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub type PositionedLexerError = PositionedObject<LexerError>;

pub enum LexerError {
    UnexpectedEOF(char),
    UnexpectedChar(char, Option<char>),
}

impl Display for LexerError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LexerError::UnexpectedEOF(missing) => write!(f, "Unexpected EOF, missing {:?}", missing)?,
            LexerError::UnexpectedChar(chr, should_be) => {
                write!(f, "Unexpected char {:?}", chr)?;
                if let Some(should_be) = should_be {
                    write!(f, ", should be {:?}", should_be)?;
                }
            },
        }
        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                              Lexer                                             //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Lexer {
    pub src: String,
    position: Position,
    current: char
}

impl Lexer {

    pub fn new(src: String) -> Lexer {
        let current = src.chars().nth(0).unwrap_or('\0');
        return Lexer {
            src,
            position: Position::default(),
            current
        }
    }

    fn advance(&mut self) {
        self.position.advance(self.current == '\n');
        self.current = self.src.chars().nth(self.position.index).unwrap_or('\0');
    }

    fn nth(&mut self, amount: usize) -> char {
        return self.src.chars().nth(self.position.index + amount).unwrap_or('\0');
    }

    fn create_single_token(&mut self, token: Token) -> PositionedToken {
        let mut end = self.position.clone();
        end.advance(self.current == '\n');
        return PositionedToken::new(
            token,
            self.position,
            end
        );
    }

    fn create_multi_token(&mut self, token: Token, size: usize) -> PositionedToken {
        let start = self.position;
        for _ in 0..size {
            self.advance();
        }
        let mut end = self.position.clone();
        end.advance(self.current == '\n');
        return PositionedToken::new(
            token,
            start,
            end
        );
    }

    fn make_identifier(&mut self) -> PositionedToken {
        let start = self.position;
        let mut str = String::new();

        while self.current.is_alphanumeric() || self.current == '_' {
            str.push(self.current);
            self.advance();
        }

        return if let Some(keyword) = Keyword::parse(str.clone()) {
            let end = self.position;
            PositionedToken::new(
                Token::Keyword(keyword),
                start,
                end
            )
        } else {
            let end = self.position;
            PositionedToken::new(
                Token::Identifier(str),
                start,
                end
            )
        }
    }

    fn make_number(&mut self) -> Result<Vec<PositionedToken>, PositionedLexerError> {
        let mut str = String::new();
        let start = self.position;
        let mut dot = false;

        loop {
            match self.current {
                '0' ..= '9' | '_' => str.push(self.current),
                '.' => {
                    if dot {
                        let err_start = self.position;
                        let mut err_end = self.position;
                        err_end.advance(self.current == '\n');
                        return Err(PositionedLexerError::new(
                            LexerError::UnexpectedChar('.', None),
                            err_start,
                            err_end,
                        ))
                    } else {
                        str.push('.');
                        dot = true;
                    }
                }
                _ => break
            }
            self.advance();
        }

        let other = match self.current {
            'a' ..= 'z' | 'A' ..= 'Z' | '_' => {
                let token = self.make_identifier();
                match &token.value {
                    Token::Keyword(keyword) => {
                        match keyword {
                            Keyword::NumberType(number_type) => {
                                let end = self.position;
                                return Ok(vec![PositionedToken::new(
                                    Token::Number(Some(number_type.clone()), str),
                                    start,
                                    end
                                )]);
                            }
                            _ => token,
                        }
                    }
                    _ => token
                }
            }
            _ => {
                let end = self.position;
                return Ok(vec![PositionedToken::new(
                    Token::Number(None, str),
                    start,
                    end
                )]);
            }
        };

        let end = self.position;
        return Ok(vec![PositionedToken::new(
            Token::Number(None, str),
            start,
            end
        ), other]);
    }

    pub fn make_char(&mut self) -> Result<PositionedToken, PositionedLexerError> {
        let start = self.position;
        let mut str = String::new();

        self.advance();
        match self.current {
            '\'' => {
                // Empty char
                self.advance();
                let end = self.position;
                return Ok(PositionedToken::new(
                    Token::Char((0 as char).to_string()),
                    start,
                    end
                ));
            }
            '\\' => {
                str.push('\\');
                self.advance();
                match self.current {
                    '\\' | '\'' | 'n' | 't' | 'r' | '0' | '"'  => str.push(self.current),
                    chr => {
                        let err_start = self.position;
                        self.advance();
                        return Err(PositionedLexerError::new(
                            LexerError::UnexpectedChar(chr, None),
                            err_start,
                            self.position
                        ));
                    }
                }
            }
            '\0' => {
                let err_start = self.position;
                self.advance();
                return Err(PositionedLexerError::new(
                    LexerError::UnexpectedEOF('"'),
                    err_start,
                    self.position
                ));
            }
            _ => str.push(self.current),
        }

        self.advance();
        return if self.current == '\'' {
            self.advance();
            Ok(PositionedToken::new(
                Token::Char(str),
                start,
                self.position
            ))
        } else {
            let chr = self.current;
            let err_start = self.position;
            self.advance();
            Err(PositionedLexerError::new(
                LexerError::UnexpectedChar(chr, Some('\'')),
                err_start,
                self.position
            ))
        }
    }

    pub fn make_string(&mut self) -> Result<PositionedToken, PositionedLexerError> {
        let start = self.position;
        let mut str = String::new();

        self.advance();

        loop {
            match self.current {
                '"' => break,
                '\\' => {
                    str.push('\\');
                    self.advance();
                    match self.current {
                        '\\' | '\'' | 'n' | 't' | 'r' | '0' | '"' => str.push(self.current),
                        chr => {
                            let err_start = self.position;
                            self.advance();
                            return Err(PositionedLexerError::new(
                                LexerError::UnexpectedChar(chr, None),
                                err_start,
                                self.position
                            ));
                        }
                    }
                }
                '\0' => {
                    let err_start = self.position;
                    self.advance();
                    return Err(PositionedLexerError::new(
                        LexerError::UnexpectedEOF('"'),
                        err_start,
                        self.position
                    ));
                }
                _ => str.push(self.current),
            }
            self.advance();
        }

        self.advance();

        return Ok(PositionedToken::new(
            Token::String(str),
            start,
            self.position
        ));
    }

    pub fn tokenize(&mut self) -> Result<Vec<PositionedToken>, PositionedLexerError> {
        let mut tokens = Vec::new();

        loop {
            match self.current {
                'a'..='z' | 'A'..='Z' | '_' => {
                    tokens.push(self.make_identifier());
                    continue; // No need to advance
                }
                '0'..='9' => {
                    tokens.append(&mut self.make_number()?);
                    continue; // No need to advance
                }
                '"' => {
                    tokens.push(self.make_string()?);
                    continue; // No need to advance
                }
                '\'' => {
                    tokens.push(self.make_char()?);
                    continue; // No need to advance
                }
                '(' => tokens.push(self.create_single_token(Token::LeftParenthesis)),
                ')' => tokens.push(self.create_single_token(Token::RightParenthesis)),
                '{' => tokens.push(self.create_single_token(Token::LeftCurlyBracket)),
                '}' => tokens.push(self.create_single_token(Token::RightCurlyBracket)),
                '[' => tokens.push(self.create_single_token(Token::LeftBracket)),
                ']' => tokens.push(self.create_single_token(Token::RightBracket)),
                ';' => tokens.push(self.create_single_token(Token::Semicolon)),
                ':' => {
                    match self.nth(1) {
                        ':' => tokens.push(self.create_multi_token(Token::DoubleColon, 1)),
                        '=' => tokens.push(self.create_multi_token(Token::ColonEqual, 1)),
                        _ => tokens.push(self.create_single_token(Token::Colon)),
                    }
                },
                ',' => tokens.push(self.create_single_token(Token::Comma)),
                '.' => tokens.push(self.create_single_token(Token::Dot)),
                '#' => {
                    while self.current != '\n' && self.current != '\0' {
                        self.advance();
                    }
                }
                '@' => tokens.push(self.create_single_token(Token::At)),
                '$' => tokens.push(self.create_single_token(Token::Dollar)),
                '+' => {
                    match self.nth(1) {
                        '=' => tokens.push(self.create_multi_token(Token::PlusEqual, 1)),
                        _ => tokens.push(self.create_single_token(Token::Plus))
                    }
                },
                '-' => {
                    match self.nth(1) {
                        '=' => tokens.push(self.create_multi_token(Token::MinusEqual, 1)),
                        _ => tokens.push(self.create_single_token(Token::Minus))
                    }
                },
                '*' => {
                    match self.nth(1) {
                        '=' => tokens.push(self.create_multi_token(Token::StarEqual, 1)),
                        _ => tokens.push(self.create_single_token(Token::Star))
                    }
                },
                '/' => {
                    match self.nth(1) {
                        '=' => tokens.push(self.create_multi_token(Token::SlashEqual, 1)),
                        '/' => {
                            while self.current != '\n' && self.current != '\0' {
                                self.advance();
                            }
                        }
                        '*' => {
                            let mut amount = 1;
                            let mut star = false;
                            let mut slash = false;
                            self.advance();
                            loop {
                                match self.current {
                                    '\0' => {
                                        let err_start = self.position;
                                        self.advance();
                                        return Err(PositionedLexerError::new(
                                            LexerError::UnexpectedEOF('*'),
                                            err_start,
                                            self.position
                                        ));
                                    }
                                    '*' => {
                                        if slash {
                                            amount += 1;
                                            slash = false;
                                        } else {
                                            star = true;
                                        }
                                    }
                                    '/' => {
                                        if star {
                                            amount -= 1;
                                            star = false;
                                            if amount == 0 {
                                                break;
                                            }
                                        } else {
                                            slash = true;
                                        }
                                    }
                                    _ => {
                                        star = false;
                                        slash = false;
                                    },
                                }
                                self.advance();
                            }
                        }
                        _ => tokens.push(self.create_single_token(Token::Slash))
                    }
                },
                '%' => {
                    match self.nth(1) {
                        '=' => tokens.push(self.create_multi_token(Token::PercentEqual, 1)),
                        _ => tokens.push(self.create_single_token(Token::Percent))
                    }
                },
                '=' => {
                    match self.nth(1) {
                        '=' => tokens.push(self.create_multi_token(Token::DoubleEqual, 1)),
                        _ => tokens.push(self.create_single_token(Token::Equal))
                    }
                },
                '&' => {
                    match self.nth(1) {
                        '=' => tokens.push(self.create_multi_token(Token::AndEqual, 1)),
                        '&' => tokens.push(self.create_multi_token(Token::DoubleAnd, 1)),
                        _ => tokens.push(self.create_single_token(Token::And))
                    }
                },
                '|' => {
                    match self.nth(1) {
                        '=' => tokens.push(self.create_multi_token(Token::PipeEqual, 1)),
                        '|' => tokens.push(self.create_multi_token(Token::DoublePipe, 1)),
                        _ => tokens.push(self.create_single_token(Token::Pipe))
                    }
                },
                '^' => {
                    match self.nth(1) {
                        '=' => tokens.push(self.create_multi_token(Token::XorEqual, 1)),
                        '&' => tokens.push(self.create_multi_token(Token::DoubleXor, 1)),
                        _ => tokens.push(self.create_single_token(Token::Xor))
                    }
                },
                '?' => {
                    match self.nth(1) {
                        '>' => tokens.push(self.create_multi_token(Token::QuestionMarkRightAngle, 1)),
                        _ => tokens.push(self.create_single_token(Token::QuestionMark))
                    }
                },
                '!' => {
                    match self.nth(1) {
                        '=' => tokens.push(self.create_multi_token(Token::ExclamationMarkEqual, 1)),
                        '>' => tokens.push(self.create_multi_token(Token::ExclamationMarkRightAngle, 1)),
                        _ => tokens.push(self.create_single_token(Token::ExclamationMark))
                    }
                },
                '<' => {
                    match self.nth(1) {
                        '=' => tokens.push(self.create_multi_token(Token::LeftAngleEqual, 1)),
                        '<' => tokens.push(self.create_multi_token(Token::DoubleLeftAngle, 1)),
                        _ => tokens.push(self.create_single_token(Token::LeftAngle))
                    }
                },
                '>' => {
                    match self.nth(1) {
                        '=' => tokens.push(self.create_multi_token(Token::RightAngleEqual, 1)),
                        '>' => tokens.push(self.create_multi_token(Token::DoubleRightAngle, 1)),
                        _ => tokens.push(self.create_single_token(Token::RightAngle))
                    }
                },
                '\0' => break,
                '\n' | '\r' | '\t' | ' ' => {
                    // Ignored
                },
                chr => {
                    let mut err_end = self.position;
                    err_end.advance(self.current == '\n');
                    return Err(PositionedLexerError::new(
                        LexerError::UnexpectedChar(chr, None),
                        self.position,
                        err_end
                    ));
                },
            }
            self.advance();
        }

        return Ok(tokens);
    }

    pub fn free(self) -> String {
        return self.src;
    }

}