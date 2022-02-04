use crate::position::PositionedObject;
use std::fmt::{Display, Formatter, Debug};

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                              Token                                             //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub type PositionedToken = PositionedObject<Token>;

#[derive(Clone, Eq, PartialEq)]
pub enum Token {
    // Word
    Keyword(Keyword),                       // Reserved Identifier
    Identifier(String),                     // Start with a-zA-Z or _ and can contains 0-9 and -
    String(String),                         // Content between ""
    Char(String),                           // Single char between ''
    Number(Option<NumberType>, String),     // Number (the type can be predefined eg: "12u8")

    // Structure
    LeftParenthesis,                        // (
    RightParenthesis,                       // )
    LeftCurlyBracket,                       // {
    RightCurlyBracket,                      // }
    LeftBracket,                            // [
    RightBracket,                           // ]
    Semicolon,                              // ;
    Colon,                                  // :
    Comma,                                  // ,
    Dot,                                    // .
    At,                                     // @
    Dollar,                                 // $

    // Operator
    Plus,                                   // +
    Minus,                                  // -
    Star,                                   // *
    Slash,                                  // /
    Percent,                                // %
    Equal,                                  // =
    And,                                    // &
    Pipe,                                   // |
    Xor,                                    // ^
    QuestionMark,                           // ?
    ExclamationMark,                        // !
    LeftAngle,                              // <
    RightAngle,                             // >

    // Combined
    DoubleColon,                            // ::
    ColonEqual,                             // :=
    PlusEqual,                              // +=
    MinusEqual,                             // -=
    StarEqual,                              // *=
    SlashEqual,                             // /=
    PercentEqual,                           // %=
    DoubleEqual,                            // ==
    AndEqual,                               // &=
    PipeEqual,                              // |=
    XorEqual,                               // ^=
    DoubleAnd,                              // &&
    DoublePipe,                             // ||
    DoubleXor,                              // ^^
    DoubleLeftAngle,                        // <<
    DoubleRightAngle,                       // >>
    LeftAngleEqual,                         // <=
    RightAngleEqual,                        // >=
    ExclamationMarkEqual,                   // !=
    QuestionMarkRightAngle,                 // ?>
    ExclamationMarkRightAngle,              // !>
}

impl Display for Token {

    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(keyword) => write!(f, "{}", keyword)?,
            Token::Identifier(id) => write!(f, "{}", id)?,
            Token::String(str) => write!(f, "\"{}\"", str)?,
            Token::Char(chr) => write!(f, "'{}'", chr)?,
            Token::Number(num_type, val) => {
                write!(f, "{}", val)?;
                if let Some(num_type) = num_type {
                    write!(f, "{}", num_type)?;
                }
            }
            Token::LeftParenthesis => write!(f, "(")?,
            Token::RightParenthesis => write!(f, ")")?,
            Token::LeftCurlyBracket => write!(f, "{{")?,
            Token::RightCurlyBracket => write!(f, "}}")?,
            Token::LeftBracket => write!(f, "[")?,
            Token::RightBracket => write!(f, "]")?,
            Token::Semicolon => write!(f, ";")?,
            Token::Colon => write!(f, ":")?,
            Token::Comma => write!(f, ",")?,
            Token::Dot => write!(f, ".")?,
            Token::At => write!(f, "@")?,
            Token::Dollar => write!(f, "$")?,
            Token::Plus => write!(f, "+")?,
            Token::Minus => write!(f, "-")?,
            Token::Star => write!(f, "*")?,
            Token::Slash => write!(f, "/")?,
            Token::Percent => write!(f, "%")?,
            Token::Equal => write!(f, "=")?,
            Token::And => write!(f, "&")?,
            Token::Pipe => write!(f, "|")?,
            Token::Xor => write!(f, "^")?,
            Token::QuestionMark => write!(f, "?")?,
            Token::ExclamationMark => write!(f, "!")?,
            Token::LeftAngle => write!(f, "<")?,
            Token::RightAngle => write!(f, ">")?,
            Token::DoubleColon => write!(f, "::")?,
            Token::ColonEqual => write!(f, ":=")?,
            Token::PlusEqual => write!(f, "+=")?,
            Token::MinusEqual => write!(f, "-=")?,
            Token::StarEqual => write!(f, "*=")?,
            Token::SlashEqual => write!(f, "/=")?,
            Token::PercentEqual => write!(f, "%=")?,
            Token::DoubleEqual => write!(f, "==")?,
            Token::AndEqual => write!(f, "&=")?,
            Token::PipeEqual => write!(f, "|=")?,
            Token::XorEqual => write!(f, "^^")?,
            Token::DoubleAnd => write!(f, "&&")?,
            Token::DoublePipe => write!(f, "||")?,
            Token::DoubleXor => write!(f, "^^")?,
            Token::DoubleLeftAngle => write!(f, "<<")?,
            Token::DoubleRightAngle => write!(f, ">>")?,
            Token::LeftAngleEqual => write!(f, "<=")?,
            Token::RightAngleEqual => write!(f, ">=")?,
            Token::ExclamationMarkEqual => write!(f, "!=")?,
            Token::QuestionMarkRightAngle => write!(f, "?>")?,
            Token::ExclamationMarkRightAngle => write!(f, "!>")?,
        }

        Ok(())
    }

}

impl Debug for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(keyword) => write!(f, "Keyword({})", keyword)?,
            Token::Identifier(_) => write!(f, "Identifier")?,
            Token::String(_) => write!(f, "String")?,
            Token::Char(_) => write!(f, "Char")?,
            Token::Number(_, _) => write!(f, "Number")?,
            Token::LeftParenthesis => write!(f, "(")?,
            Token::RightParenthesis => write!(f, ")")?,
            Token::LeftCurlyBracket => write!(f, "{{")?,
            Token::RightCurlyBracket => write!(f, "}}")?,
            Token::LeftBracket => write!(f, "[")?,
            Token::RightBracket => write!(f, "]")?,
            Token::Semicolon => write!(f, ";")?,
            Token::Colon => write!(f, ":")?,
            Token::Comma => write!(f, ",")?,
            Token::Dot => write!(f, ".")?,
            Token::At => write!(f, "@")?,
            Token::Dollar => write!(f, "$")?,
            Token::Plus => write!(f, "+")?,
            Token::Minus => write!(f, "-")?,
            Token::Star => write!(f, "*")?,
            Token::Slash => write!(f, "/")?,
            Token::Percent => write!(f, "%")?,
            Token::Equal => write!(f, "=")?,
            Token::And => write!(f, "&")?,
            Token::Pipe => write!(f, "|")?,
            Token::Xor => write!(f, "^")?,
            Token::QuestionMark => write!(f, "?")?,
            Token::ExclamationMark => write!(f, "!")?,
            Token::LeftAngle => write!(f, "<")?,
            Token::RightAngle => write!(f, ">")?,
            Token::DoubleColon => write!(f, "::")?,
            Token::ColonEqual => write!(f, ":=")?,
            Token::PlusEqual => write!(f, "+=")?,
            Token::MinusEqual => write!(f, "-=")?,
            Token::StarEqual => write!(f, "*=")?,
            Token::SlashEqual => write!(f, "/=")?,
            Token::PercentEqual => write!(f, "%=")?,
            Token::DoubleEqual => write!(f, "==")?,
            Token::AndEqual => write!(f, "&=")?,
            Token::PipeEqual => write!(f, "|=")?,
            Token::XorEqual => write!(f, "^=")?,
            Token::DoubleAnd => write!(f, "&&")?,
            Token::DoublePipe => write!(f, "||")?,
            Token::DoubleXor => write!(f, "^^")?,
            Token::DoubleLeftAngle => write!(f, "<<")?,
            Token::DoubleRightAngle => write!(f, ">>")?,
            Token::LeftAngleEqual => write!(f, "<=")?,
            Token::RightAngleEqual => write!(f, ">=")?,
            Token::ExclamationMarkEqual => write!(f, "!=")?,
            Token::QuestionMarkRightAngle => write!(f, "?>")?,
            Token::ExclamationMarkRightAngle => write!(f, "!>")?,
        }

        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                             Keyword                                            //
////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Keyword {
    // Operators
    In,
    To,
    Is,
    Exact,
    Extends,
    And,
    Or,
    Xor,
    Not,
    // Variables
    Var,
    Let,
    Const,
    Alias,
    Null,
    // Built-in types
    NumberType(NumberType),
    Char,
    Str,
    Bool,
    // Bool Value
    True,
    False,
    // Reference
    Ref,
    Deref,
    // Option
    Unwrap,
    UnwrapOr,
    // Function
    Fn,
    // Flow
    If,
    Else,
    Elif,
    For,
    While,
    Goto,
    Continue,
    Break,
    Return,
    // Modifier
    Global,
    Pub,
    Prot,
    // Group
    Group,
    // Objects
    Class,
    SELF,                                   // Supposed to be Self but it's a reserved Keyword
    New,
    Intf,
    Proto,
    Enum,
    // Compiler Instructions
    Call,
    Uncheck,
    Unreachable,
    Import,
    Include,
    Generic,
    Lifetime,
    Free
}

impl Keyword {

    pub fn parse(str: String) -> Option<Keyword> {
        match str.as_str() {
            "in" => Some(Keyword::In),
            "to" => Some(Keyword::To),
            "is" => Some(Keyword::Is),
            "exact" => Some(Keyword::Exact),
            "extends" => Some(Keyword::Extends),
            "and" => Some(Keyword::And),
            "or" => Some(Keyword::Or),
            "xor" => Some(Keyword::Xor),
            "not" => Some(Keyword::Not),
            "var" => Some(Keyword::Var),
            "let" => Some(Keyword::Let),
            "const" => Some(Keyword::Const),
            "alias" => Some(Keyword::Alias),
            "null" => Some(Keyword::Null),
            "u8" => Some(Keyword::NumberType(NumberType::U8)),
            "u16" => Some(Keyword::NumberType(NumberType::U16)),
            "u32" => Some(Keyword::NumberType(NumberType::U32)),
            "u64" => Some(Keyword::NumberType(NumberType::U64)),
            "i8" => Some(Keyword::NumberType(NumberType::I8)),
            "i16" => Some(Keyword::NumberType(NumberType::I16)),
            "i32" => Some(Keyword::NumberType(NumberType::I32)),
            "i64" => Some(Keyword::NumberType(NumberType::I64)),
            "usize" => Some(Keyword::NumberType(NumberType::Usize)),
            "isize" => Some(Keyword::NumberType(NumberType::Isize)),
            "f32" => Some(Keyword::NumberType(NumberType::F32)),
            "f64" => Some(Keyword::NumberType(NumberType::F64)),
            "char" => Some(Keyword::Char),
            "str" => Some(Keyword::Str),
            "bool" => Some(Keyword::Bool),
            "true" => Some(Keyword::True),
            "false" => Some(Keyword::False),
            "ref" => Some(Keyword::Ref),
            "deref" => Some(Keyword::Deref),
            "unwrap" => Some(Keyword::Unwrap),
            "unwrap_or" => Some(Keyword::UnwrapOr),
            "fn" => Some(Keyword::Fn),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "elif" => Some(Keyword::Elif),
            "for" => Some(Keyword::For),
            "while" => Some(Keyword::While),
            "goto" => Some(Keyword::Goto),
            "continue" => Some(Keyword::Continue),
            "break" => Some(Keyword::Break),
            "return" => Some(Keyword::Return),
            "global" => Some(Keyword::Global),
            "pub" => Some(Keyword::Pub),
            "prot" => Some(Keyword::Prot),
            "group" => Some(Keyword::Group),
            "class" => Some(Keyword::Class),
            "self" => Some(Keyword::SELF),
            "new" => Some(Keyword::New),
            "intf" => Some(Keyword::Intf),
            "proto" => Some(Keyword::Proto),
            "enum" => Some(Keyword::Enum),
            "call" => Some(Keyword::Call),
            "uncheck" => Some(Keyword::Uncheck),
            "unreachable" => Some(Keyword::Unreachable),
            "import" => Some(Keyword::Import),
            "include" => Some(Keyword::Include),
            "generic" => Some(Keyword::Generic),
            "lifetime" => Some(Keyword::Lifetime),
            "free" => Some(Keyword::Free),
            _ => None
        }
    }

}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::In => write!(f, "in")?,
            Keyword::To => write!(f, "to")?,
            Keyword::Is => write!(f, "is")?,
            Keyword::Exact => write!(f, "exact")?,
            Keyword::Extends => write!(f, "extends")?,
            Keyword::And => write!(f, "and")?,
            Keyword::Or => write!(f, "or")?,
            Keyword::Xor => write!(f, "xor")?,
            Keyword::Not => write!(f, "not")?,
            Keyword::Var => write!(f, "var")?,
            Keyword::Let => write!(f, "let")?,
            Keyword::Const => write!(f, "const")?,
            Keyword::Alias => write!(f, "alias")?,
            Keyword::Null => write!(f, "null")?,
            Keyword::NumberType(val) => write!(f, "{}", val)?,
            Keyword::Char => write!(f, "char")?,
            Keyword::Str => write!(f, "str")?,
            Keyword::Bool => write!(f, "bool")?,
            Keyword::True => write!(f, "true")?,
            Keyword::False => write!(f, "false")?,
            Keyword::Ref => write!(f, "ref")?,
            Keyword::Deref => write!(f, "deref")?,
            Keyword::Unwrap => write!(f, "unwrap")?,
            Keyword::UnwrapOr => write!(f, "unwrap_or")?,
            Keyword::Fn => write!(f, "fn")?,
            Keyword::If => write!(f, "if")?,
            Keyword::Else => write!(f, "else")?,
            Keyword::Elif => write!(f, "elif")?,
            Keyword::For => write!(f, "for")?,
            Keyword::While => write!(f, "while")?,
            Keyword::Goto => write!(f, "goto")?,
            Keyword::Continue => write!(f, "continue")?,
            Keyword::Break => write!(f, "break")?,
            Keyword::Return => write!(f, "return")?,
            Keyword::Global => write!(f, "global")?,
            Keyword::Pub => write!(f, "pub")?,
            Keyword::Prot => write!(f, "prot")?,
            Keyword::Group => write!(f, "group")?,
            Keyword::Class => write!(f, "class")?,
            Keyword::SELF => write!(f, "self")?,
            Keyword::New => write!(f, "new")?,
            Keyword::Intf => write!(f, "intf")?,
            Keyword::Proto => write!(f, "proto")?,
            Keyword::Enum => write!(f, "enum")?,
            Keyword::Call => write!(f, "call")?,
            Keyword::Uncheck => write!(f, "uncheck")?,
            Keyword::Unreachable => write!(f, "unreachable")?,
            Keyword::Import => write!(f, "import")?,
            Keyword::Include => write!(f, "include")?,
            Keyword::Generic => write!(f, "generic")?,
            Keyword::Lifetime => write!(f, "lifetime")?,
            Keyword::Free => write!(f, "free")?,
        }

        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                           Number Type                                          //
////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum NumberType {
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    Usize,
    Isize,
    F32,
    F64
}

impl Display for NumberType {

    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            NumberType::U8 => write!(f, "u8")?,
            NumberType::U16 => write!(f, "u16")?,
            NumberType::U32 => write!(f, "u32")?,
            NumberType::U64 => write!(f, "u64")?,
            NumberType::I8 => write!(f, "i8")?,
            NumberType::I16 => write!(f, "i16")?,
            NumberType::I32 => write!(f, "i32")?,
            NumberType::I64 => write!(f, "i64")?,
            NumberType::Usize => write!(f, "usize")?,
            NumberType::Isize => write!(f, "isize")?,
            NumberType::F32 => write!(f, "f32")?,
            NumberType::F64 => write!(f, "f64")?,
        }

        Ok(())
    }

}

