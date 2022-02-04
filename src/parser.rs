use crate::position::{EmptyPositioned, Position, Positioned, PositionedObject, PositionedString};
use std::fmt::{Display, Formatter};
use std::process::id;
use crate::token::{PositionedToken, Token, Keyword};
use crate::node::{Node, PositionedNode, ValueNode, Operator, VarType, PositionedVarType, PositionedValueType, ValueType, PositionedIdentifier, Identifier, PositionedOperator, PositionedParameterDef, ParameterDef, PositionedParameterCall, ParameterCall, ConditionBranch, PositionedAccessModifier, AccessModifier};

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                          Parser Error                                          //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub type PositionedParserError = PositionedObject<ParserError>;

pub enum ParserError {
    UnexpectedEOF(Option<String>),
    UnexpectedToken(Token, Option<String>)
}

impl Display for ParserError {

    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedEOF(should_be) => {
                write!(f, "Unexpected EOF")?;
                if let Some(should_be) = should_be {
                    write!(f, ", should be '{}'", should_be)?;
                }
            }
            ParserError::UnexpectedToken(token, should_be) => {
                write!(f, "Unexpected token '{}'", token)?;
                if let Some(should_be) = should_be {
                    write!(f, ", should be '{}'", should_be)?;
                }
            }
        }

        Ok(())
    }

}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                             Parser                                             //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub struct Parser {
    tokens: Vec<PositionedToken>,
    index: usize,
}

impl Parser {

    pub fn new(tokens: Vec<PositionedToken>) -> Parser {
        return Parser {
            tokens,
            index: 0
        }
    }

    fn get_current(&self) -> Option<&PositionedToken> {
        return self.tokens.get(self.index);
    }

    fn nth(&self, amount: usize) -> Option<&PositionedToken> {
        return self.tokens.get(self.index + amount);
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn advance_x(&mut self, x: usize) {
        self.index += x;
    }

    fn rewind(&mut self) {
        self.index -= 1;
    }

    fn rewind_x(&mut self, x: usize) {
        self.index -= x;
    }

    /////////////////////
    // Error functions //
    /////////////////////

    fn unexpected_token(token: PositionedToken, should_be: Option<Token>) -> PositionedParserError {
        return PositionedParserError::new(
            ParserError::UnexpectedToken(token.value, if let Some(should_be) = should_be { Some(format!("{:?}", should_be)) } else { None }),
            token.start,
            token.end,
        )
    }

    fn unexpected_eof(should_be: Option<Token>) -> PositionedParserError {
        return PositionedParserError::new(
            ParserError::UnexpectedEOF(if let Some(should_be) = should_be { Some(format!("{:?}", should_be)) } else { None }),
            Position::eof(),
            Position::eof(),
        )
    }

    fn unexpected_token_str(token: PositionedToken, should_be: Option<String>) -> PositionedParserError {
        return PositionedParserError::new(
            ParserError::UnexpectedToken(token.value, should_be),
            token.start,
            token.end,
        )
    }

    fn unexpected_eof_str(should_be: Option<String>) -> PositionedParserError {
        return PositionedParserError::new(
            ParserError::UnexpectedEOF(should_be),
            Position::eof(),
            Position::eof(),
        )
    }

    /////////////////////
    // Check functions //
    /////////////////////

    fn expect_token(&self, expected: Token) -> Result<&PositionedToken, PositionedParserError> {
        return if let Some(current) = self.get_current() {
            if current.value == expected {
                Ok(current)
            } else {
                Err(Self::unexpected_token(current.clone(), Some(expected)))
            }
        } else {
            Err(Self::unexpected_eof(Some(expected)))
        };
    }

    fn expect_current(&self, should_be: Option<Token>) -> Result<&PositionedToken, PositionedParserError> {
        return if let Some(current) = self.get_current() {
            Ok(current)
        } else {
            Err(Self::unexpected_eof(should_be))
        };
    }

    fn expect_current_str(&self, should_be: Option<String>) -> Result<&PositionedToken, PositionedParserError> {
        return if let Some(current) = self.get_current() {
            Ok(current)
        } else {
            Err(Self::unexpected_eof_str(should_be))
        };
    }

    /////////////////////
    // Parse functions //
    /////////////////////

    fn parse_variable_assignment(&mut self, identifier: PositionedIdentifier) -> Result<PositionedNode, PositionedParserError> {
        let start = identifier.start;
        self.advance();
        self.expect_token(Token::Equal)?;
        self.advance();
        let expr = self.parse_expr()?;
        self.rewind(); // (Should end on last token)
        let end = expr.end;
        return Ok(PositionedNode::new(
            Node::VariableAssignment {
                name: identifier,
                value: Box::new(expr)
            },
            start,
            end
        ));
    }

    fn parse_value(&mut self) -> Result<PositionedNode, PositionedParserError> {
        let current = self.expect_current(None)?.clone();
        match current.value.clone() {
            Token::String(value) => Ok(current.convert(Node::Value(ValueNode::String(value)))),
            Token::Char(value) => Ok(current.convert(Node::Value(ValueNode::Char(value)))),
            Token::Number(number_type, value) => Ok(current.convert(Node::Value(ValueNode::Number(number_type, value)))),
            Token::LeftParenthesis => {
                let start = current.start;
                self.advance();
                let mut parts = Vec::new();
                let mut allow_next = true;
                let end;
                loop {
                    let current = self.expect_current(Some(Token::RightParenthesis))?;
                    match current.value {
                        Token::Comma => {
                            if !allow_next {
                                allow_next = true;
                                self.advance();
                            } else {
                                return Err(Self::unexpected_token_str(current.clone(), Some("Expression".to_string())));
                            }
                        }
                        Token::RightParenthesis => {
                            if !allow_next {
                                end = current.end;
                                break;
                            } else {
                                return Err(Self::unexpected_token_str(current.clone(), Some("Expression".to_string())));
                            }
                        }
                        _ => {
                            if allow_next {
                                let expr = self.parse_expr()?;
                                parts.push(expr);
                                allow_next = false;
                            } else {
                                return Err(Self::unexpected_token(current.clone(), Some(Token::Comma)));
                            }
                        }
                    }
                }

                return match parts.len() {
                    1 => {
                        let mut body = parts.remove(0);
                        body.start = start;
                        body.end = end;
                        Ok(body)
                    }
                    _ => {
                        Ok(PositionedNode::new(
                            Node::Value(ValueNode::Tuple(parts)),
                            start,
                            end
                        ))
                    }
                };
            }
            Token::LeftCurlyBracket => {
                let start = current.start;
                self.advance();
                let mut values = Vec::new();
                let end;
                loop {
                    values.push(self.parse_expr()?);
                    let current = self.expect_current(Some(Token::RightCurlyBracket))?;
                    match current.value {
                        Token::Comma => self.advance(),
                        Token::RightCurlyBracket => {
                            end = current.end;
                            break
                        },
                        _ => return Err(Self::unexpected_token_str(current.clone(), Some("{ or ,".to_string()))),
                    }
                }
                Ok(PositionedNode::new(
                    Node::Value(ValueNode::Array(values)),
                    start,
                    end
                ))
            }
            Token::Identifier(name) => {
                let mut id = current.convert(Identifier::root(name));
                let start = id.start;
                loop {
                    if let Some(mut next) = self.nth(1) {
                        match next.value {
                            Token::Equal => return self.parse_variable_assignment(id),
                            Token::DoubleColon => {
                                if let Some(next2) = self.nth(2) {
                                    match next2.value.clone() {
                                        Token::LeftAngle => {
                                            self.advance_x(2); // Aligned on left_angle
                                            let node = self.parse_function_call(id, start);
                                            self.rewind();
                                            return node;
                                        }
                                        Token::Identifier(name) => {
                                            // Identifier::Identifier...
                                            id = PositionedIdentifier::new(
                                                Identifier::with_parent(id, name),
                                                start,
                                                next2.end
                                            );
                                            self.advance_x(2);
                                        }
                                        _ => break,
                                    }
                                }
                            }
                            Token::LeftParenthesis => {
                                self.advance(); // Align with left parenthesis
                                let node = self.parse_function_call(id, start);
                                self.rewind();
                                return node;
                            }
                            _ => break,
                        }
                    } else {
                        break;
                    }
                }
                // Variable Call
                Ok(current.convert(Node::VariableCall {
                    name: id
                }))
            }
            Token::Keyword(keyword) => {
                match keyword {
                    Keyword::Null => Ok(current.convert(Node::Value(ValueNode::Null))),
                    Keyword::True => Ok(current.convert(Node::Value(ValueNode::Bool(true)))),
                    Keyword::False => Ok(current.convert(Node::Value(ValueNode::Bool(false)))),
                    _ => Err(Self::unexpected_token(current.clone(), None)),
                }
            }
            _ => return Err(Self::unexpected_token_str(current.clone(), Some("Value".to_string()))),
        }
    }

    fn parse_op0(&mut self) -> Result<PositionedNode, PositionedParserError> {
        let mut left = self.parse_value()?;
        self.advance();

        loop {
            if let Some(current) = self.get_current() {
                match current.value {
                    Token::LeftBracket => {
                        let start = left.start;
                        let op_start = current.start;
                        self.advance();
                        let value = self.parse_expr()?;
                        let end = self.expect_token(Token::RightBracket)?.end;
                        left = PositionedNode::new(
                            Node::BinaryOp {
                                left: Box::new(left),
                                operator: PositionedOperator::new(
                                    Operator::Indexing,
                                    op_start,
                                    end,
                                ),
                                right: Box::new(value),
                            },
                            start,
                            end
                        );
                    }
                    Token::QuestionMark => {
                        let op_start = current.start;
                        if let Some(next) = self.nth(1) {
                            match next.value {
                                Token::LeftBracket => {
                                    let start = left.start;
                                    self.advance_x(2);
                                    let value = self.parse_expr()?;
                                    let end = self.expect_token(Token::RightBracket)?.end;
                                    left = PositionedNode::new(
                                        Node::BinaryOp {
                                            left: Box::new(left),
                                            operator: PositionedOperator::new(
                                                Operator::SafeIndexing,
                                                op_start,
                                                end,
                                            ),
                                            right: Box::new(value),
                                        },
                                        start,
                                        end
                                    );
                                }
                                _ => break,
                            }
                        } else {
                            break;
                        }
                    }
                    _ => break,
                }
            } else {
                break;
            }

            self.advance();
        }

        return Ok(left);
    }

    fn parse_op1(&mut self) -> Result<PositionedNode, PositionedParserError> {
        let current = self.expect_current(None)?;

        match current.value.clone() {
            Token::Plus => {
                let start = current.start;
                let op_tok = current.clone();
                self.advance();
                let value = self.parse_op1()?;
                let end = value.end;
                return Ok(PositionedNode::new(
                    Node::UnaryOp {
                        operator: op_tok.convert(Operator::Minus),
                        value: Box::new(value)
                    },
                    start,
                    end
                ));
            }
            Token::Minus => {
                let start = current.start;
                let op_tok = current.clone();
                self.advance();
                let value = self.parse_op1()?;
                let end = value.end;
                return Ok(PositionedNode::new(
                    Node::UnaryOp {
                        operator: op_tok.convert(Operator::Minus),
                        value: Box::new(value)
                    },
                    start,
                    end
                ));
            }
            Token::ExclamationMark => {
                let start = current.start;
                let op_tok = current.clone();
                self.advance();
                let value = self.parse_op1()?;
                let end = value.end;
                return Ok(PositionedNode::new(
                    Node::UnaryOp {
                        operator: op_tok.convert(Operator::Not),
                        value: Box::new(value)
                    },
                    start,
                    end
                ));
            }
            Token::Keyword(keyword) => {
                match keyword {
                    Keyword::Not => {
                        let start = current.start;
                        let op_tok = current.clone();
                        self.advance();
                        let value = self.parse_op1()?;
                        let end = value.end;
                        return Ok(PositionedNode::new(
                            Node::UnaryOp {
                                operator: op_tok.convert(Operator::Not),
                                value: Box::new(value)
                            },
                            start,
                            end
                        ));
                    }
                    Keyword::Ref => {
                        let start = current.start;
                        let op_tok = current.clone();
                        self.advance();
                        let value = self.parse_op1()?;
                        let end = value.end;
                        return Ok(PositionedNode::new(
                            Node::UnaryOp {
                                operator: op_tok.convert(Operator::Ref),
                                value: Box::new(value)
                            },
                            start,
                            end
                        ));
                    }
                    Keyword::Deref => {
                        let start = current.start;
                        let op_tok = current.clone();
                        self.advance();
                        let value = self.parse_op1()?;
                        let end = value.end;
                        return Ok(PositionedNode::new(
                            Node::UnaryOp {
                                operator: op_tok.convert(Operator::Deref),
                                value: Box::new(value)
                            },
                            start,
                            end
                        ));
                    }
                    _ => {}
                }
            }
            _ => {}
        }

        return self.parse_op0();
    }

    fn parse_op2(&mut self) -> Result<PositionedNode, PositionedParserError> {
        let mut left = self.parse_op1()?;

        loop {
            if let Some(current) = self.get_current() {
                match current.value {
                    Token::Keyword(keyword) => {
                        match keyword {
                            Keyword::To => {
                                let op = current.convert(Operator::To);
                                self.advance();
                                let right = self.parse_type()?;
                                self.advance();
                                let start = left.start();
                                let value = right.value.clone();
                                let end = right.end();
                                left = PositionedNode::new(
                                    Node::BinaryOp {
                                        left: Box::new(left),
                                        operator: op,
                                        right: Box::new(right.convert(Node::Value(ValueNode::Type(value)))),
                                    },
                                    start,
                                    end
                                );
                                continue;
                            }
                            _ => break,
                        }
                    }
                    _ => break,
                };
            } else {
                break;
            }
        }

        return Ok(left);
    }

    fn parse_op3(&mut self) -> Result<PositionedNode, PositionedParserError> {
        if let Some(current) = self.get_current() {
            match current.value {
                Token::Keyword(Keyword::Unwrap) => {
                    let start = current.start;
                    let op_tok = current.clone();
                    self.advance();
                    let value = self.parse_op2()?;
                    let end = value.end;
                    return Ok(PositionedNode::new(
                        Node::UnaryOp {
                            operator: op_tok.convert(Operator::UnsafeUnwrap),
                            value: Box::new(value)
                        },
                        start,
                        end
                    ));
                }
                _ => {}
            }
        }

        let mut left = self.parse_op2()?;

        loop {
            if let Some(current) = self.get_current() {
                let op = match current.value.clone() {
                    Token::QuestionMarkRightAngle | Token::Keyword(Keyword::UnwrapOr) => current.convert(Operator::UnwrapOr),
                    Token::ExclamationMarkRightAngle => {
                        let start = left.start;
                        let end = current.end;
                        left = PositionedNode::new(
                            Node::UnaryOp {
                                operator: current.convert(Operator::UnsafeUnwrap),
                                value: Box::new(left),
                            },
                            start,
                            end,
                        );
                        self.advance();
                        continue;
                    },
                    Token::QuestionMark => {
                        let start = left.start;
                        let end = current.end;
                        left = PositionedNode::new(
                            Node::UnaryOp {
                                operator: current.convert(Operator::Check),
                                value: Box::new(left),
                            },
                            start,
                            end,
                        );
                        self.advance();
                        continue;
                    }
                    _ => break,
                };
                self.advance();

                let right = self.parse_op2()?;
                let start = left.start();
                let end = right.end();
                left = PositionedNode::new(
                    Node::BinaryOp {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    },
                    start,
                    end
                );
            } else {
                break;
            }
        }

        return Ok(left);
    }

    fn parse_op4(&mut self) -> Result<PositionedNode, PositionedParserError> {
        // [factor]([[*]|[/]|[%]])|[factor]]?*
        let mut left = self.parse_op3()?;

        loop {
            if let Some(current) = self.get_current() {
                let op = match current.value {
                    Token::Star => current.convert(Operator::Multiply),
                    Token::Slash => current.convert(Operator::Divide),
                    Token::Percent => current.convert(Operator::Remainder),
                    _ => break,
                };
                self.advance();

                let right = self.parse_op3()?;
                let start = left.start();
                let end = right.end();
                left = PositionedNode::new(
                    Node::BinaryOp {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    },
                    start,
                    end
                );
            } else {
                break;
            }
        }

        return Ok(left);
    }

    fn parse_op5(&mut self) -> Result<PositionedNode, PositionedParserError> {
        // [term]([[+]|[-]|[<<]|[>>]|[&]|[|]|[^]])|[term]]?*
        let mut left = self.parse_op4()?;

        loop {
            if let Some(current) = self.get_current() {
                let op = match current.value {
                    Token::Plus => current.convert(Operator::Plus),
                    Token::Minus => current.convert(Operator::Minus),
                    Token::DoubleLeftAngle => current.convert(Operator::LeftShift),
                    Token::DoubleRightAngle => current.convert(Operator::RightShift),
                    Token::And => current.convert(Operator::BitAnd),
                    Token::Pipe => current.convert(Operator::BitOr),
                    Token::Xor => current.convert(Operator::BitXor),
                    _ => break,
                };
                self.advance();

                let right = self.parse_op4()?;
                let start = left.start();
                let end = right.end();
                left = PositionedNode::new(
                    Node::BinaryOp {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    },
                    start,
                    end
                );
            } else {
                break;
            }
        }

        return Ok(left);
    }

    fn parse_op6(&mut self) -> Result<PositionedNode, PositionedParserError> {
        // [expr]([[<]|[>]|[<=]|[>=]|[==]|[!=]])|[expr]]?*
        let mut left = self.parse_op5()?;

        loop {
            if let Some(current) = self.get_current() {
                let op = match current.value {
                    Token::LeftAngle => current.convert(Operator::Less),
                    Token::RightAngle => current.convert(Operator::Greater),
                    Token::LeftAngleEqual => current.convert(Operator::LessOrEqual),
                    Token::RightAngleEqual => current.convert(Operator::GreaterOrEqual),
                    Token::DoubleEqual => current.convert(Operator::Equal),
                    Token::ExclamationMarkEqual => current.convert(Operator::NotEqual),
                    Token::Colon => current.convert(Operator::Range(false)),
                    Token::ColonEqual => current.convert(Operator::Range(true)),
                    Token::Keyword(Keyword::In) => current.convert(Operator::In),
                    _ => break,
                };
                self.advance();

                let right = self.parse_op5()?;
                let start = left.start();
                let end = right.end();
                left = PositionedNode::new(
                    Node::BinaryOp {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    },
                    start,
                    end
                );
            } else {
                break;
            }
        }

        return Ok(left);
    }

    fn parse_expr(&mut self) -> Result<PositionedNode, PositionedParserError> {
        // [cmp]([[&&]|[||]|[^^]])|[cmp]]?*
        let mut left = self.parse_op6()?;

        loop {
            if let Some(current) = self.get_current() {
                let op = match current.value {
                    Token::DoubleAnd => current.convert(Operator::And),
                    Token::DoublePipe => current.convert(Operator::Or),
                    Token::DoubleXor => current.convert(Operator::Xor),
                    Token::Keyword(keyword) => {
                        match keyword {
                            Keyword::And => current.convert(Operator::And),
                            Keyword::Or => current.convert(Operator::Or),
                            Keyword::Xor => current.convert(Operator::Xor),
                            _ => break
                        }
                    }
                    _ => break,
                };
                self.advance();

                let right = self.parse_op6()?;
                let start = left.start();
                let end = right.end();
                left = PositionedNode::new(
                    Node::BinaryOp {
                        left: Box::new(left),
                        operator: op,
                        right: Box::new(right),
                    },
                    start,
                    end
                );
            } else {
                break;
            }
        }

        return Ok(left);
    }

    fn parse_type(&mut self) -> Result<PositionedValueType, PositionedParserError> {
        let current = self.expect_current_str(Some("Type".to_string()))?;
        match &current.value {
            Token::Keyword(keyword) => {
                match keyword {
                    Keyword::NumberType(num_type) => Ok(current.convert(ValueType::Number(num_type.clone()))),
                    Keyword::Char => Ok(current.convert(ValueType::Char)),
                    Keyword::Str => Ok(current.convert(ValueType::String)),
                    Keyword::Bool => Ok(current.convert(ValueType::Bool)),
                    _ => Err(Self::unexpected_token_str(current.clone(), Some("Type".to_string()))),
                }
            }
            Token::Identifier(id) => Ok(current.convert(ValueType::Custom(id.clone()))),
            Token::QuestionMark => { // [?][Type]
                let start = current.start;
                self.advance();
                let inner = self.parse_type()?;
                let end = inner.end;
                Ok(PositionedValueType::new(
                    ValueType::Option(Box::new(inner)),
                    start,
                    end,
                ))
            }
            Token::And => { // [&][Type]
                let start = current.start;
                self.advance();
                let inner = self.parse_type()?;
                let end = inner.end;
                Ok(PositionedValueType::new(
                    ValueType::Reference(Box::new(inner)),
                    start,
                    end,
                ))
            }
            Token::LeftBracket => { // ([)[bool_op](])[Type]
                let start = current.start;
                self.advance();
                let size = self.parse_expr()?;
                self.expect_token(Token::RightBracket)?;
                self.advance();
                let inner = self.parse_type()?;
                let end = inner.end;
                Ok(PositionedValueType::new(
                    ValueType::Array(Box::new(size), Box::new(inner)),
                    start,
                    end,
                ))
            }
            Token::LeftParenthesis => {
                let start = current.start;
                self.advance();
                let mut parts = Vec::new();
                let mut allow_next = true;
                let end;
                loop {
                    let current = self.expect_current(Some(Token::RightParenthesis))?;
                    match current.value {
                        Token::Comma => {
                            if !allow_next {
                                allow_next = true;
                                self.advance();
                            } else {
                                return Err(Self::unexpected_token_str(current.clone(), Some("Type".to_string())));
                            }
                        }
                        Token::RightParenthesis => {
                            if !allow_next || parts.is_empty() {
                                end = current.end;
                                break;
                            } else {
                                return Err(Self::unexpected_token_str(current.clone(), Some("Type".to_string())));
                            }
                        }
                        _ => {
                            let value_type = self.parse_type()?;
                            self.advance();
                            parts.push(value_type);
                            allow_next = false;
                        }
                    }
                }

                return Ok(PositionedValueType::new(
                    ValueType::Tuple(parts),
                    start,
                    end,
                ));
            }
            _ => Err(Self::unexpected_token_str(current.clone(), Some("Type".to_string()))),
        }
    }

    fn parse_variable_def(&mut self, var_type: PositionedVarType, access: Option<PositionedAccessModifier>, global: Option<EmptyPositioned>, start: Position) -> Result<PositionedNode, PositionedParserError> {
        // Name
        let name = {
            let current = self.expect_current_str(Some("Identifier".to_string()))?;
            match current.value.clone() {
                Token::Identifier(name) => current.convert(name),
                _ => return Err(Self::unexpected_token_str(current.clone(), Some("Identifier".to_string())))
            }
        };
        self.advance();
        let mut end = name.end;

        // Value Type
        let value_type = {
            let current = self.expect_current(Some(Token::Semicolon))?;
            match current.value.clone() {
                Token::Colon => {
                    self.advance();
                    let value_type = self.parse_type()?;
                    end = value_type.end;
                    self.advance();
                    Some(value_type)
                }
                _ => None
            }
        };

        // Value
        let value = {
            let current = self.expect_current(Some(Token::Semicolon))?;
            match current.value.clone() {
                Token::Equal => {
                    self.advance();
                    let value = self.parse_expr()?;
                    end = value.end;
                    Some(Box::new(value))
                }
                _ => None
            }
        };

        return Ok(PositionedNode::new(
            Node::VariableDefinition {
                var_type,
                name,
                value_type,
                value,
                access,
                global
            },
            start,
            end
        ));
    }

    fn parse_function_def(&mut self, access: Option<PositionedAccessModifier>, global: Option<EmptyPositioned>, start: Position) -> Result<PositionedNode, PositionedParserError> {
        // Identifier
        let mut current = self.expect_current_str(Some("Identifier".to_string()))?.clone();
        let name = if let Token::Identifier(name) = &current.value {
            current.convert(name.clone())
        } else {
            return Err(Self::unexpected_token_str(current.clone(), Some("Identifier".to_string())));
        };
        self.advance();

        // Generics
        let mut generics = Vec::new();
        current = self.expect_current(Some(Token::LeftParenthesis))?.clone();
        if current.value == Token::LeftAngle {
            self.advance();
            let mut allow_next = true;
            loop {
                current = self.expect_current(Some(Token::RightAngle))?.clone();
                match current.value.clone() {
                    Token::Comma => {
                        if !allow_next {
                            allow_next = true;
                            self.advance();
                        } else {
                            return Err(Self::unexpected_token_str(current.clone(), Some("Identifier".to_string())));
                        }
                    }
                    Token::Identifier(gen) => {
                        if allow_next {
                            let gen_id = current.convert(gen);
                            self.advance();
                            allow_next = false;
                            generics.push(gen_id);
                        } else {
                            return Err(Self::unexpected_token(current.clone(), Some(Token::Comma)));
                        }
                    }
                    Token::RightAngle => {
                        if !allow_next || generics.is_empty() {
                            self.advance();
                            break;
                        } else {
                            return Err(Self::unexpected_token_str(current.clone(), Some("Identifier".to_string())));
                        }
                    }
                    _ => return Err(Self::unexpected_token(current.clone(), Some(Token::RightAngle))),
                }
            }
        }

        // Parameters
        self.expect_token(Token::LeftParenthesis)?;
        self.advance();
        let mut allow_next = true;
        let mut params = Vec::new();
        loop {
            current = self.expect_current(Some(Token::RightParenthesis))?.clone();
            match current.value.clone() {
                Token::Comma => {
                    if !allow_next {
                        allow_next = true;
                        self.advance();
                    } else {
                        return Err(Self::unexpected_token_str(current.clone(), Some("Identifier".to_string())));
                    }
                }
                Token::Identifier(name) => {
                    if allow_next {
                        let start = current.start;
                        let param_id = current.convert(name);
                        allow_next = false;
                        self.advance();
                        self.expect_token(Token::Colon)?;
                        self.advance();
                        let param_type = self.parse_type()?;
                        self.advance();
                        let mut end = current.end;
                        current = self.expect_current(Some(Token::RightParenthesis))?.clone();
                        let default_value = if current.value == Token::Pipe {
                            self.advance();
                            let expr = self.parse_expr()?;
                            end = expr.end;
                            Some(expr)
                        } else {
                            None
                        };
                        params.push(PositionedParameterDef::new(
                            ParameterDef {
                                name: param_id,
                                value_type: param_type,
                                default_value
                            },
                            start,
                            end
                        ));
                    } else {
                        return Err(Self::unexpected_token(current.clone(), Some(Token::Comma)));
                    }
                }
                Token::RightParenthesis => {
                    if !allow_next || params.is_empty() {
                        self.advance();
                        current = self.expect_current(Some(Token::LeftCurlyBracket))?.clone();
                        break;
                    } else {
                        return Err(Self::unexpected_token_str(current.clone(), Some("Identifier".to_string())));
                    }
                }
                 _ => return Err(Self::unexpected_token(current.clone(), Some(Token::RightBracket))),
            }
        }

        // Type
        let return_type = if current.value == Token::Colon {
            self.advance();
            let return_type = self.parse_type()?;
            self.advance();
            Some(return_type)
        } else {
            None
        };

        // Body
        let end;
        self.expect_token(Token::LeftCurlyBracket)?;
        self.advance();
        let mut body = vec![];
        loop {
            current = self.expect_current(Some(Token::RightCurlyBracket))?.clone();
            if current.value == Token::RightCurlyBracket {
                end = current.end;
                self.advance();
                break;
            } else {
                let node = self.parse_current()?;
                body.push(node);
            }
        }

        return Ok(PositionedNode::new(
            Node::FunctionDefinition {
                name,
                generics,
                parameters: params,
                return_type,
                body,
                access,
                global
            },
            start,
            end,
        ));
    }

    fn parse_function_call(&mut self, name: PositionedIdentifier, start: Position) -> Result<PositionedNode, PositionedParserError> {
        let mut current = self.get_current().expect("#P1: Should not happen").clone();
        let mut generics = Vec::new();
        if current.value == Token::LeftAngle {
            self.advance();
            let mut allow_next = true;
            loop {
                current = self.expect_current(Some(Token::RightAngle))?.clone();
                match current.value {
                    Token::Comma => {
                        if !allow_next {
                            allow_next = true;
                            self.advance();
                        } else {
                            return Err(Self::unexpected_token_str(current, Some("Type".to_string())));
                        }
                    }
                    Token::RightAngle => {
                        if !allow_next || generics.is_empty() {
                            self.advance();
                            break;
                        } else {
                            return Err(Self::unexpected_token_str(current, Some("Type".to_string())));
                        }
                    }
                    _ => {
                        let generic = self.parse_type()?;
                        generics.push(generic);
                        self.advance();
                        allow_next = false;
                    }
                }
            }
        }

        self.expect_token(Token::LeftParenthesis)?;
        self.advance();
        let mut allow_next = true;
        let mut params = Vec::new();
        let mut current;
        loop {
            current = self.expect_current(Some(Token::RightParenthesis))?;

            match current.value {
                Token::Comma => {
                    if !allow_next {
                        allow_next = true;
                        self.advance();
                    } else {
                        return Err(Self::unexpected_token_str(current.clone(), Some("Identifier".to_string())));
                    }
                }
                Token::RightParenthesis => {
                    if !allow_next || params.is_empty() {
                        self.advance();
                        break;
                    } else {
                        return Err(Self::unexpected_token_str(current.clone(), Some("Identifier".to_string())));
                    }
                }
                _ => {
                    if allow_next {
                        let start = current.start;
                        if let Token::Identifier(name) = &current.value {
                            let id = current.convert(name.clone());
                            if let Some(next) = self.nth(1) {
                                if next.value == Token::Equal {
                                    self.advance_x(2);
                                    let expr = self.parse_expr()?;
                                    let end = expr.end;
                                    allow_next = false;
                                    params.push(PositionedParameterCall::new(
                                        ParameterCall {
                                            name: Some(id),
                                            value: expr
                                        },
                                        start,
                                        end
                                    ));
                                    continue;
                                }
                            } else {
                                return Err(Self::unexpected_eof(Some(Token::RightParenthesis)));
                            }
                        }
                        let expr = self.parse_expr()?;
                        let end = expr.end;
                        allow_next = false;
                        params.push(PositionedParameterCall::new(
                            ParameterCall {
                                name: None,
                                value: expr
                            },
                            start,
                            end
                        ));
                    } else {
                        return Err(Self::unexpected_token(current.clone(), Some(Token::Comma)));
                    }
                }
            }
        }

        let end = self.get_current().unwrap().end;

        return Ok(PositionedNode::new(
            Node::FunctionCall {
                name,
                generics,
                parameters: params,
            },
            start,
            end,
        ));
    }

    fn parse_return(&mut self) -> Result<PositionedNode, PositionedParserError> {
        let start = self.get_current().unwrap().start;
        self.advance();
        let expr = self.parse_expr()?;
        let end = expr.end;
        return Ok(PositionedNode::new(
            Node::Return(Box::new(expr)),
            start,
            end,
        ));
    }

    fn parse_if(&mut self) -> Result<PositionedNode, PositionedParserError> {
        let start = self.get_current().unwrap().start;
        self.advance();

        // If Condition
        self.expect_token(Token::LeftParenthesis)?;
        self.advance();
        let condition = self.parse_expr()?;
        self.expect_token(Token::RightParenthesis)?;
        self.advance();

        let mut current = self.expect_current(Some(Token::LeftCurlyBracket))?;
        let mut capture = None;
        if current.value == Token::Pipe {
            self.advance();
            current = self.expect_current_str(Some("Identifier".to_string()))?;
            if let Token::Identifier(name) = &current.value {
                capture = Some(current.convert(name.clone()));
            } else {
                return Err(Self::unexpected_token_str(current.clone(), Some("Identifier".to_string())));
            }
            self.advance();
            self.expect_token(Token::Pipe)?;
            self.advance();
        }

        // If Body
        self.expect_token(Token::LeftCurlyBracket)?;
        self.advance();
        let mut body = vec![];
        let mut end;
        loop {
            current = self.expect_current(Some(Token::RightCurlyBracket))?;
            if current.value == Token::RightCurlyBracket {
                end = current.end;
                self.advance();
                break;
            } else {
                let node = self.parse_current()?;
                body.push(node);
            }
        }

        let if_condition = ConditionBranch {
            condition,
            body,
            capture,
        };

        let mut elif_branch = Vec::new();
        let mut else_body = Vec::new();
        loop {
            if let Some(mut current) = self.get_current() {
                match current.value {
                    Token::Keyword(Keyword::Else) => {
                        self.advance();
                        current = self.expect_current(Some(Token::LeftCurlyBracket))?;
                        match current.value {
                            Token::LeftCurlyBracket => {
                                self.advance();
                                let mut current;
                                loop {
                                    current = self.expect_current(Some(Token::RightCurlyBracket))?.clone();
                                    if current.value == Token::RightCurlyBracket {
                                        end = current.end;
                                        self.advance();
                                        break;
                                    } else {
                                        let node = self.parse_current()?;
                                        else_body.push(node);
                                    }
                                }
                                break;
                            }
                            Token::Keyword(Keyword::If) => {
                                self.advance();
                            }
                            _ => return Err(Self::unexpected_token(current.clone(), Some(Token::LeftCurlyBracket))),
                        }
                    }
                    Token::Keyword(Keyword::Elif) => {
                        self.advance();
                    }
                    _ => break
                }

                // Else if / Elif
                self.expect_token(Token::LeftParenthesis)?;
                self.advance();
                let condition = self.parse_expr()?;
                self.expect_token(Token::RightParenthesis)?;
                self.advance();

                current = self.expect_current(Some(Token::LeftCurlyBracket))?;
                let mut capture = None;
                if current.value == Token::Pipe {
                    self.advance();
                    current = self.expect_current_str(Some("Identifier".to_string()))?;
                    if let Token::Identifier(name) = &current.value {
                        capture = Some(current.convert(name.clone()));
                    } else {
                        return Err(Self::unexpected_token_str(current.clone(), Some("Identifier".to_string())));
                    }
                    self.advance();
                    self.expect_token(Token::Pipe)?;
                    self.advance();
                }

                // Body
                self.expect_token(Token::LeftCurlyBracket)?;
                self.advance();
                let mut body = vec![];
                let mut current;
                loop {
                    current = self.expect_current(Some(Token::RightCurlyBracket))?.clone();
                    if current.value == Token::RightCurlyBracket {
                        end = current.end;
                        self.advance();
                        break;
                    } else {
                        let node = self.parse_current()?;
                        body.push(node);
                    }
                }

                let branch = ConditionBranch {
                    condition,
                    body,
                    capture,
                };
                elif_branch.push(branch);
            } else {
                break;
            }
        }

        return Ok(PositionedNode::new(
            Node::If {
                if_branch: Box::new(if_condition),
                elif_branch,
                else_body,
            },
            start,
            end
        ));
    }

    fn parse_while(&mut self) -> Result<PositionedNode, PositionedParserError> {
        let start = self.get_current().unwrap().start;
        self.advance();

        // Condition
        self.expect_token(Token::LeftParenthesis)?;
        self.advance();
        let condition = self.parse_expr()?;
        self.expect_token(Token::RightParenthesis)?;
        self.advance();

        // Capture
        let mut current = self.expect_current(Some(Token::LeftCurlyBracket))?;
        let capture = if current.value == Token::Pipe {
            self.advance();
            current = self.expect_current_str(Some("Identifier".to_string()))?;
            if let Token::Identifier(name) = &current.value {
                let name = current.convert(name.clone());
                self.advance();
                self.expect_token(Token::Pipe);
                self.advance();
                Some(name)
            } else {
                return Err(Self::unexpected_token_str(current.clone(), Some("Identifier".to_string())));
            }
        } else {
            None
        };

        // Body
        self.expect_token(Token::LeftCurlyBracket)?;
        self.advance();
        let mut body = vec![];
        let mut current;
        let end;
        loop {
            current = self.expect_current(Some(Token::RightCurlyBracket))?.clone();
            if current.value == Token::RightCurlyBracket {
                end = current.end;
                self.advance();
                break;
            } else {
                let node = self.parse_current()?;
                body.push(node);
            }
        }

        return Ok(PositionedNode::new(
            Node::While(Box::new(ConditionBranch {
                condition,
                body,
                capture,
            })),
            start,
            end,
        ));
    }

    fn parse_for(&mut self) -> Result<PositionedNode, PositionedParserError> {
        let start = self.get_current().unwrap().start;
        self.advance();

        // Condition
        self.expect_token(Token::LeftParenthesis)?;
        self.advance();
        let mut current = self.expect_current_str(Some("Identifier".to_string()))?;
        let var_name;
        if let Token::Identifier(identifier) = &current.value {
            var_name = current.convert(identifier.clone());
            self.advance();
        } else {
            return Err(Self::unexpected_token_str(current.clone(), Some("Identifier".to_string())));
        }
        self.expect_token(Token::Keyword(Keyword::In))?;
        self.advance();
        let rhs = self.parse_expr()?;
        self.expect_token(Token::RightParenthesis)?;
        self.advance();

        // Body
        self.expect_token(Token::LeftCurlyBracket)?;
        self.advance();
        let mut body = vec![];
        let mut current;
        let end;
        loop {
            current = self.expect_current(Some(Token::RightCurlyBracket))?.clone();
            if current.value == Token::RightCurlyBracket {
                end = current.end;
                self.advance();
                break;
            } else {
                let node = self.parse_current()?;
                body.push(node);
            }
        }

        return Ok(PositionedNode::new(
            Node::ForEach {
                var_name,
                right: Box::new(rhs),
                body
            },
            start,
            end,
        ));
    }

    fn parse_label_str(&mut self) -> Result<PositionedString, PositionedParserError> {
        self.expect_token(Token::Dollar)?;
        self.advance();
        let current = self.expect_current_str(Some("Identifier".to_string()))?;
        return if let Token::Identifier(name) = &current.value {
            let name = current.convert(name.clone());
            self.advance();
            Ok(name)
        } else {
            Err(Self::unexpected_token_str(current.clone(), Some("Identifier".to_string())))
        }
    }

    fn parse_label(&mut self) -> Result<PositionedNode, PositionedParserError> {
        let start = self.get_current().unwrap().start;
        let label = self.parse_label_str()?;
        let end = label.end;
        return Ok(PositionedNode::new(
            Node::Label(label),
            start,
            end
        ));
    }

    fn parse_goto(&mut self) -> Result<PositionedNode, PositionedParserError> {
        let start = self.get_current().unwrap().start;
        self.advance();
        let label = self.parse_label_str()?;
        let end = label.end;

        return Ok(PositionedNode::new(
            Node::Goto(label),
            start,
            end,
        ));
    }

    fn parse_continue(&mut self) -> Result<PositionedNode, PositionedParserError> {
        let start = self.get_current().unwrap().start;
        let mut end = self.get_current().unwrap().end;
        self.advance();
        let current = self.expect_current(Some(Token::Semicolon))?;
        return if current.value == Token::Dollar {
            let label = self.parse_label_str()?;
            end = label.end;

            Ok(PositionedNode::new(
                Node::Continue(Some(label)),
                start,
                end,
            ))
        } else {
            Ok(PositionedNode::new(
                Node::Continue(None),
                start,
                end,
            ))
        }
    }

    fn parse_break(&mut self) -> Result<PositionedNode, PositionedParserError> {
        let start = self.get_current().unwrap().start;
        let mut end = self.get_current().unwrap().end;
        self.advance();
        let current = self.expect_current(Some(Token::Semicolon))?;
        return if current.value == Token::Dollar {
            let label = self.parse_label_str()?;
            end = label.end;

            Ok(PositionedNode::new(
                Node::Break(Some(label)),
                start,
                end,
            ))
        } else {
            Ok(PositionedNode::new(
                Node::Break(None),
                start,
                end,
            ))
        }
    }

    fn parse_alias(&mut self, access: Option<PositionedAccessModifier>, global: Option<EmptyPositioned>, start: Position) -> Result<PositionedNode, PositionedParserError> {
        // Name
        let current = self.expect_current_str(Some("Identifier".to_string()))?;
        let name;
        if let Token::Identifier(identifier) = &current.value {
            name = current.convert(identifier.clone());
            self.advance();
        } else {
            return Err(Self::unexpected_token_str(current.clone(), Some("Identifier".to_string())));
        }

        self.expect_token(Token::Equal);
        self.advance();

        // Type
        let value_type = self.parse_type()?;
        let end = value_type.end;
        self.advance();

        return Ok(PositionedNode::new(
            Node::Alias {
                name,
                value: value_type,
                access,
                global
            },
            start,
            end
        ));
    }

    fn parse_group(&mut self, start: Position) -> Result<PositionedNode, PositionedParserError> {
        let mut current = self.expect_current_str(Some("Identifier".to_string()))?.clone();
        return if let Token::Identifier(name) = current.value.clone() {
            let id = current.convert(name);
            self.advance();
            self.expect_token(Token::LeftCurlyBracket)?;
            self.advance();
            let mut nodes = Vec::new();
            loop {
                current = self.expect_current(Some(Token::RightCurlyBracket))?.clone();
                if current.value == Token::RightCurlyBracket {
                    self.advance();
                    break;
                } else {
                    let node = self.parse_current()?;
                    nodes.push(node);
                }
            }

            let end = current.end;

            Ok(PositionedNode::new(
                Node::Group {
                    name: id,
                    body: nodes
                },
                start,
                end,
            ))
        } else {
            Err(Self::unexpected_token_str(current, Some("Identifier".to_string())))
        };
    }

    fn process_modifier(&mut self, access: Option<PositionedAccessModifier>, global: Option<EmptyPositioned>, start: Position) -> Result<PositionedNode, PositionedParserError> {
        let current = self.expect_current(None)?.clone();
        return match current.value {
            // Variable Definition
            Token::Keyword(Keyword::Var) => {
                self.advance();
                let node = self.parse_variable_def(self.get_current().unwrap().convert(VarType::Var), access, global, start)?;
                self.expect_token(Token::Semicolon)?;
                self.advance();
                Ok(node)
            }
            Token::Keyword(Keyword::Let) => {
                self.advance();
                let node = self.parse_variable_def(self.get_current().unwrap().convert(VarType::Let), access, global, start)?;
                self.expect_token(Token::Semicolon)?;
                self.advance();
                Ok(node)
            }
            Token::Keyword(Keyword::Const) => {
                self.advance();
                let node = self.parse_variable_def(self.get_current().unwrap().convert(VarType::Const), access, global, start)?;
                self.expect_token(Token::Semicolon)?;
                self.advance();
                Ok(node)
            }
            // Alias
            Token::Keyword(Keyword::Alias) => {
                self.advance();
                let node = self.parse_alias(access, global, start)?;
                self.expect_token(Token::Semicolon)?;
                self.advance();
                Ok(node)
            }
            // Function Definition
            Token::Keyword(Keyword::Fn) => {
                self.advance();
                self.parse_function_def(access, global, start)
            }
            // Access
            Token::Keyword(Keyword::Pub) => {
                if access.is_some() {
                    return Err(Self::unexpected_token(current.clone(), None));
                }

                self.advance();
                self.process_modifier(Some(current.convert(AccessModifier::Public)), global, start)
            }
            Token::Keyword(Keyword::Prot) => {
                if access.is_some() {
                    return Err(Self::unexpected_token(current.clone(), None));
                }

                self.advance();
                self.process_modifier(Some(current.convert(AccessModifier::Protected)), global, start)
            }
            // Global
            Token::Keyword(Keyword::Global) => {
                if global.is_some() {
                    return Err(Self::unexpected_token(current.clone(), None));
                }

                self.advance();
                self.process_modifier(access, Some(current.convert(())), start)
            }
            _ => Err(Self::unexpected_token(current.clone(), None)),
        }
    }

    fn process_keyword(&mut self, keyword: Keyword) -> Result<PositionedNode, PositionedParserError> {
        let current = self.get_current().expect("Should not happen").clone();
        let start = current.start;
        match keyword {
            Keyword::Var => {
                self.advance();
                let node = self.parse_variable_def(self.get_current().unwrap().convert(VarType::Var), None, None, start)?;
                self.expect_token(Token::Semicolon)?;
                self.advance();
                return Ok(node);
            },
            Keyword::Let => {
                self.advance();
                let node = self.parse_variable_def(self.get_current().unwrap().convert(VarType::Let), None, None, start)?;
                self.expect_token(Token::Semicolon)?;
                self.advance();
                return Ok(node);
            },
            Keyword::Const => {
                self.advance();
                let node = self.parse_variable_def(self.get_current().unwrap().convert(VarType::Const), None, None, start)?;
                self.expect_token(Token::Semicolon)?;
                self.advance();
                return Ok(node);
            },
            Keyword::Alias => {
                self.advance();
                let node = self.parse_alias(None, None, start)?;
                self.expect_token(Token::Semicolon)?;
                self.advance();
                return Ok(node);
            }
            Keyword::Unwrap | Keyword::Ref | Keyword::Deref | Keyword::Not | Keyword::Null | Keyword::True | Keyword::False => {
                let node = self.parse_expr()?;
                self.expect_token(Token::Semicolon)?;
                self.advance();
                return Ok(node);
            },
            Keyword::Fn => {
                self.advance();
                return self.parse_function_def(None, None, start);
            },
            Keyword::Return => {
                let node = self.parse_return()?;
                self.expect_token(Token::Semicolon)?;
                self.advance();
                return Ok(node);
            }
            Keyword::If => return self.parse_if(),
            Keyword::For => return self.parse_for(),
            Keyword::While => return self.parse_while(),
            Keyword::Goto => {
                let node = self.parse_goto()?;
                self.expect_token(Token::Semicolon)?;
                self.advance();
                return Ok(node);
            }
            Keyword::Continue => {
                let node = self.parse_continue()?;
                self.expect_token(Token::Semicolon)?;
                self.advance();
                return Ok(node);
            }
            Keyword::Break => {
                let node = self.parse_break()?;
                self.expect_token(Token::Semicolon)?;
                self.advance();
                return Ok(node);
            }
            Keyword::Global => {
                self.advance();
                return self.process_modifier(None, Some(current.convert(())), start);
            }
            Keyword::Pub => {
                self.advance();
                return self.process_modifier(Some(current.convert(AccessModifier::Public)), None, start);
            }
            Keyword::Prot => {
                self.advance();
                return self.process_modifier(Some(current.convert(AccessModifier::Protected)), None, start);
            }
            Keyword::Group => {
                self.advance();
                return self.parse_group(start);
            }
            Keyword::Class => {
                todo!()
            }
            Keyword::SELF => {
                todo!()
            }
            Keyword::New => {
                todo!()
            }
            Keyword::Intf => {
                todo!()
            }
            Keyword::Proto => {
                todo!()
            }
            Keyword::Enum => {
                todo!()
            }
            _ => todo!("Should not happen"),
        }
    }

    fn parse_call(&mut self, start: Position) -> Result<PositionedNode, PositionedParserError> {
        self.advance();
        self.expect_token(Token::LeftParenthesis)?;
        self.advance();
        let mut current = self.expect_current_str(Some("Identifier".to_string()))?.clone();
        return if let Token::String(language) = current.value.clone() {
            let language = current.convert(language);
            self.advance();
            self.expect_token(Token::RightParenthesis)?;
            self.advance();
            let node = self.parse_expr()?;
            let end = self.expect_token(Token::Semicolon)?.end;
            self.advance();
            Ok(PositionedNode::new(
                Node::CICall {
                    language,
                    function: Box::new(node),
                },
                start,
                end,
            ))
        } else {
            Err(Self::unexpected_token_str(current, Some("Identifier".to_string())))
        }
    }

    fn parse_uncheck(&mut self, start: Position) -> Result<PositionedNode, PositionedParserError> {
        self.advance();
        self.expect_token(Token::LeftCurlyBracket)?;
        self.advance();
        let mut nodes = Vec::new();
        let mut current;
        loop {
            current = self.expect_current(Some(Token::RightCurlyBracket))?.clone();
            if current.value == Token::RightCurlyBracket {
                self.advance();
                break;
            } else {
                let node = self.parse_current()?;
                nodes.push(node);
            }
        }
        let end = current.end;
        return Ok(PositionedNode::new(
            Node::CIUncheck(nodes),
            start,
            end
        ));
    }

    fn parse_unreachable(&mut self, start: Position) -> Result<PositionedNode, PositionedParserError> {
        self.advance();
        let end = self.expect_token(Token::Semicolon)?.end;
        self.advance();
        return Ok(PositionedNode::new(
            Node::CIUnreachable,
            start,
            end
        ));
    }

    fn parse_import(&mut self, start: Position) -> Result<PositionedNode, PositionedParserError> {
        self.advance();
        let end = self.expect_token(Token::LeftParenthesis);
        self.advance();
        let mut current = self.expect_current_str(Some("Identifier".to_string()))?.clone();
        return if let Token::Identifier(name) = current.value.clone() {
            let mut id = current.convert(Identifier::root(name));
            self.advance();
            let mut allow_next = false;
            loop {
                current = self.expect_current(Some(Token::RightParenthesis))?.clone();
                match current.value.clone() {
                    Token::Identifier(name) => {
                        if allow_next {
                            let id_start = id.start;
                            let id_end = current.end;
                            id = PositionedIdentifier::new(
                                Identifier::with_parent(id, name),
                                id_start,
                                id_end
                            );
                            self.advance();
                            allow_next = false;
                        } else {
                            return Err(Self::unexpected_token(current, Some(Token::DoubleColon)));
                        }
                    }
                    Token::DoubleColon => {
                        if !allow_next {
                            allow_next = true;
                            self.advance();
                        } else {
                            return Err(Self::unexpected_token_str(current, Some("Identifier".to_string())));
                        }
                    }
                    _ => {
                        if !allow_next {
                            break
                        } else {
                            return Err(Self::unexpected_token_str(current, Some("Identifier".to_string())));
                        }
                    },
                }
            }

            let mut alias = None;
            if current.value == Token::Comma {
                self.advance();
                current = self.expect_current_str(Some("Identifier".to_string()))?.clone();
                if let Token::Identifier(alias_id) = current.value.clone() {
                    alias = Some(current.convert(alias_id));
                    self.advance();
                    current = self.expect_current(Some(Token::RightParenthesis))?.clone();
                } else {
                    return Err(Self::unexpected_token_str(current, Some("Identifier".to_string())));
                }
            }

            self.expect_token(Token::RightParenthesis)?;
            self.advance();
            let end = self.expect_token(Token::Semicolon)?.end;
            self.advance();
            Ok(PositionedNode::new(
                Node::CIImport {
                    library: id,
                    alias,
                },
                start,
                end,
            ))
        } else {
            Err(Self::unexpected_token_str(current, Some("Identifier".to_string())))
        }
    }

    fn parse_include(&mut self, start: Position) -> Result<PositionedNode, PositionedParserError> {
        todo!()
    }

    fn parse_generic(&mut self, start: Position) -> Result<PositionedNode, PositionedParserError> {
        self.advance();
        self.expect_token(Token::LeftParenthesis)?;
        self.advance();
        let mut generics = Vec::new();
        let mut allow_next = true;
        let mut current;
        loop {
            current = self.expect_current(Some(Token::RightParenthesis))?.clone();
            match current.value {
                Token::Comma => {
                    if !allow_next {
                        allow_next = true;
                        self.advance();
                    } else {
                        return Err(Self::unexpected_token_str(current, Some("Type".to_string())));
                    }
                }
                Token::RightParenthesis => {
                    if !allow_next | generics.is_empty() {
                        self.advance();
                        break;
                    } else {
                        return Err(Self::unexpected_token_str(current, Some("Type".to_string())));
                    }
                }
                _ => {
                    let generic = self.parse_type()?;
                    self.advance();
                    generics.push(generic);
                    allow_next = false;
                }
            }
        }

        current = self.expect_current(Some(Token::Keyword(Keyword::Fn)))?.clone();
        let expr = self.parse_expr()?;
        let end = self.expect_token(Token::Semicolon)?.end;
        self.advance();
        return Ok(PositionedNode::new(
            Node::CIGeneric {
                generics,
                function: Box::new(expr),
            },
            start,
            end,
        ));
    }

    fn parse_lifetime(&mut self, start: Position) -> Result<PositionedNode, PositionedParserError> {
        todo!()
    }

    fn parse_free(&mut self, start: Position) -> Result<PositionedNode, PositionedParserError> {
        todo!()
    }

    fn parse_compiler_instruction(&mut self, start: Position) -> Result<PositionedNode, PositionedParserError> {
        self.advance();
        let current = self.expect_current_str(Some("Compiler instruction".to_string()))?;
        return if let Token::Keyword(keyword) = current.value {
            match keyword {
                Keyword::Call => self.parse_call(start),
                Keyword::Uncheck => self.parse_uncheck(start),
                Keyword::Unreachable => self.parse_unreachable(start),
                Keyword::Import => self.parse_import(start),
                Keyword::Include => self.parse_include(start),
                Keyword::Generic => self.parse_generic(start),
                Keyword::Lifetime => self.parse_lifetime(start),
                Keyword::Free => self.parse_free(start),
                _ => Err(Self::unexpected_token_str(current.clone(), Some("Compiler instruction".to_string()))),
            }
        } else {
            Err(Self::unexpected_token_str(current.clone(), Some("Compiler instruction".to_string())))
        }
    }

    fn parse_current(&mut self) -> Result<PositionedNode, PositionedParserError> {
        if let Some(current) = self.get_current() {
            match current.value {
                Token::Identifier(_) | Token::LeftParenthesis |
                Token::Plus | Token::Minus | Token::ExclamationMark |
                Token::String(_) | Token::Char(_) | Token::Number(_, _) => {
                    let node = self.parse_expr()?;
                    self.expect_token(Token::Semicolon)?;
                    self.advance();
                    return Ok(node);
                },
                Token::Keyword(keyword) => return self.process_keyword(keyword),
                Token::Dollar => return self.parse_label(),
                Token::At => return self.parse_compiler_instruction(current.start),
                _ => panic!("Unexpected Token: {}", self.get_current().unwrap())
            }
        } else {
            panic!("Unexpected Token: {}", self.get_current().unwrap())
        }
    }

    pub fn parse(&mut self) -> Result<Vec<PositionedNode>, PositionedParserError> {
        let mut ast = Vec::new();

        loop {
            if self.get_current().is_some() {
                ast.push(self.parse_current()?);
            } else {
                break;
            }
        }

        return Ok(ast);
    }

}