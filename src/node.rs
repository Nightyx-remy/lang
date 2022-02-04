use crate::position::{EmptyPositioned, PositionedObject, PositionedString};
use crate::token::NumberType;
use std::fmt::{Display, Formatter, Debug};
use std::ptr::write;

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                              Node                                              //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub type PositionedNode = PositionedObject<Node>;

#[derive(Clone)]
pub enum Node {
    Value (ValueNode),
    UnaryOp {
        operator: PositionedOperator,
        value: Box<PositionedNode>,
    },
    BinaryOp {
        left: Box<PositionedNode>,
        operator: PositionedOperator,
        right: Box<PositionedNode>
    },
    VariableDefinition {
        var_type: PositionedVarType,
        name: PositionedString,
        value_type: Option<PositionedValueType>,
        value: Option<Box<PositionedNode>>,
        access: Option<PositionedAccessModifier>,
        global: Option<EmptyPositioned>,
    },
    VariableCall {
        name: PositionedIdentifier
    },
    VariableAssignment {
        name: PositionedIdentifier,
        value: Box<PositionedNode>,
    },
    FunctionDefinition {
        name: PositionedString,
        generics: Vec<PositionedString>,
        parameters: Vec<PositionedParameterDef>,
        return_type: Option<PositionedValueType>,
        body: Vec<PositionedNode>,
        access: Option<PositionedAccessModifier>,
        global: Option<EmptyPositioned>,
    },
    FunctionCall {
        name: PositionedIdentifier,
        generics: Vec<PositionedValueType>,
        parameters: Vec<PositionedParameterCall>
    },
    Return(Box<PositionedNode>),
    If {
        if_branch: Box<ConditionBranch>,
        elif_branch: Vec<ConditionBranch>,
        else_body: Vec<PositionedNode>,   // Empty = no else body
    },
    ForEach {
        var_name: PositionedString,
        right: Box<PositionedNode>,
        body: Vec<PositionedNode>,
    },
    While (Box<ConditionBranch>),
    Label(PositionedString),
    Goto(PositionedString),
    Continue(Option<PositionedString>),
    Break(Option<PositionedString>),
    Alias {
        name: PositionedString,
        value: PositionedValueType,
        access: Option<PositionedAccessModifier>,
        global: Option<EmptyPositioned>,
    },
    Group {
        name: PositionedString,
        body: Vec<PositionedNode>,
    },
    CICall {
        language: PositionedString,
        function: Box<PositionedNode>,
    },
    CIUncheck(Vec<PositionedNode>),
    CIUnreachable,
    CIImport {
        library: PositionedIdentifier,
        alias: Option<PositionedString>,
    },
    CIGeneric {
        generics: Vec<PositionedValueType>,
        function: Box<PositionedNode>,
    },
}

impl Display for Node {

    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Node::Value(value) => write!(f, "{}", value)?,
            Node::UnaryOp { operator, value } => write!(f, "({}{})", operator, value)?,
            Node::BinaryOp { left, operator, right } => {
                match operator.value {
                    Operator::Indexing => write!(f, "({}[{}])", left, right)?,
                    Operator::SafeIndexing => write!(f, "({}?[{}])", left, right)?,
                    _ => write!(f, "({} {} {})", left, operator, right)?
                }
            },
            Node::VariableDefinition { var_type, name, value_type, value, access, global } => {
                // Access
                if let Some(access) = access {
                    write!(f, "{}", access)?;
                } else {
                    write!(f, "{}", AccessModifier::Private)?;
                }
                // Global
                if let Some(_) = global {
                    write!(f, " global")?;
                }
                // Variable
                write!(f, " {} {}", var_type, name)?;
                // Type
                if let Some(value_type) = value_type {
                    write!(f, ": {}", value_type)?;
                }
                // Value
                if let Some(value) = value {
                    write!(f, " = {}", value)?;
                }
            }
            Node::VariableCall { name } => write!(f, "{}", name)?,
            Node::VariableAssignment { name, value } => write!(f, "{} = {}", name, value)?,
            Node::FunctionDefinition { name, generics, parameters, return_type, body, access, global } => {
                // Access
                if let Some(access) = access {
                    write!(f, "{}", access)?;
                } else {
                    write!(f, "{}", AccessModifier::Private)?;
                }
                // Global
                if let Some(_) = global {
                    write!(f, " global")?;
                }
                // Function
                write!(f, " fn {}", name)?;
                if !generics.is_empty() {
                    write!(f, "<")?;
                    let mut index = 0;
                    for generic in generics.iter() {
                        if index != 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", generic)?;
                        index += 1;
                    }
                    write!(f, ">")?;
                }
                write!(f, "(")?;
                let mut index = 0;
                for param in parameters.iter() {
                    if index != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                    index += 1;
                }
                write!(f, ")")?;
                if let Some(return_type) = return_type {
                    write!(f, ": {}", return_type)?;
                }
                write!(f, " {{")?;
                for node in body {
                    for line in node.to_string().lines() {
                        write!(f, "\n\t{}", line)?;
                    }
                }
                write!(f, "\n}}")?;
            }
            Node::FunctionCall { name, generics, parameters } => {
                write!(f, "{}", name)?;
                if !generics.is_empty() {
                    write!(f, "<")?;
                    let mut index = 0;
                    for generic in generics {
                        if index != 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", generic)?;
                        index += 1;
                    }
                    write!(f, ">")?;
                }
                write!(f, "(")?;
                let mut index = 0;
                for param in parameters {
                    if index != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", param)?;
                    index += 1;
                }
                write!(f, ")")?;
            }
            Node::Return(value) => write!(f, "return {}", value)?,
            Node::If { if_branch, elif_branch, else_body } => {
                write!(f, "if ({}) ", if_branch.condition)?;
                if let Some(capture) = &if_branch.capture {
                    write!(f, "|{}| ", capture)?;
                }
                write!(f, "{{")?;
                for node in if_branch.body.iter() {
                    for line in node.to_string().lines() {
                        write!(f, "\n\t{}", line)?;
                    }
                }
                write!(f, "\n}} ")?;
                for elif in elif_branch {
                    write!(f, "elif ({}) ", elif.condition)?;
                    if let Some(capture) = &elif.capture {
                        write!(f, "|{}| ", capture)?;
                    }
                    write!(f, "{{")?;
                    for node in elif.body.iter() {
                        for line in node.to_string().lines() {
                            write!(f, "\n\t{}", line)?;
                        }
                    }
                    write!(f, "\n}} ")?;
                }

                if !else_body.is_empty() {
                    write!(f, "else {{")?;
                    for node in else_body.iter() {
                        for line in node.to_string().lines() {
                            write!(f, "\n\t{}", line)?;
                        }
                    }
                    write!(f, "\n}} ")?;
                }
            }
            Node::ForEach { var_name, right, body } => {
                write!(f, "for ({} in {}) {{", var_name, right);
                for node in body.iter() {
                    for line in node.to_string().lines() {
                        write!(f, "\n\t{}", line)?;
                    }
                }
                write!(f, "\n}} ")?;
            }
            Node::While(condition_branch) => {
                write!(f, "while ({}) ", condition_branch.condition)?;
                if let Some(capture) = &condition_branch.capture {
                    write!(f, "|{}| ", capture)?;
                }
                write!(f, "{{")?;
                for node in condition_branch.body.iter() {
                    for line in node.to_string().lines() {
                        write!(f, "\n\t{}", line)?;
                    }
                }
                write!(f, "\n}} ")?;
            }
            Node::Label(name) => write!(f, "${}", name)?,
            Node::Goto(label) => write!(f, "goto ${}", label)?,
            Node::Continue(label) => {
                write!(f, "continue")?;
                if let Some(label) = label {
                    write!(f, " {}", label);
                }
            }
            Node::Break(label) => {
                write!(f, "break")?;
                if let Some(label) = label {
                    write!(f, " {}", label);
                }
            }
            Node::Alias { name, value, access, global } => {
                // Access
                if let Some(access) = access {
                    write!(f, "{}", access)?;
                } else {
                    write!(f, "{}", AccessModifier::Private)?;
                }
                // Global
                if let Some(_) = global {
                    write!(f, " global")?;
                }
                // Alias
                write!(f, " alias {} = {}", name, value)?
            },
            Node::Group { name, body } => {
                write!(f, "group {} {{", name)?;

                for node in body {
                    for line in node.to_string().lines() {
                        write!(f, "\n\t{}", line)?;
                    }
                }

                write!(f, "\n}}")?;
            }
            Node::CICall { language, function } => write!(f, "@call({}) {}", language, function)?,
            Node::CIUncheck(nodes) => {
                write!(f, "@uncheck {{")?;

                for node in nodes {
                    for line in node.to_string().lines() {
                        write!(f, "\n\t{}", line)?;
                    }
                }

                write!(f, "\n}}")?;
            }
            Node::CIUnreachable => write!(f, "@unreachable")?,
            Node::CIImport { library, alias } => {
                write!(f, "@import({}", library)?;
                if let Some(alias) = alias {
                    write!(f, ", {}", alias)?;
                }
                write!(f, ")")?;
            }
            Node::CIGeneric { generics, function } => {
                write!(f, "@generic(")?;
                let mut index = 0;
                for generic in generics {
                    if index != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", generic)?;
                    index += 1;
                }
                write!(f, ") {}", function);
            }
        }

        Ok(())
    }

}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                         Access Modifier                                        //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub type PositionedAccessModifier = PositionedObject<AccessModifier>;

#[derive(Clone)]
pub enum AccessModifier {
    Public,
    Protected,
    Private,
}

impl Display for AccessModifier {

    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            AccessModifier::Public => write!(f, "public")?,
            AccessModifier::Protected => write!(f, "protected")?,
            AccessModifier::Private => write!(f, "private")?,
        }
        Ok(())
    }

}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                           Identifier                                           //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub type PositionedIdentifier = PositionedObject<Identifier>;

#[derive(Clone)]
pub struct Identifier {
    pub parent: Option<Box<PositionedIdentifier>>,
    pub value: String,
}

impl Identifier {

    pub fn with_parent(parent: PositionedIdentifier, value: String) -> Identifier {
        return Identifier {
            parent: Some(Box::new(parent)),
            value,
        }
    }

    pub fn root(value: String) -> Identifier {
        return Identifier {
            parent: None,
            value
        }
    }

}

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(parent) = &self.parent {
            write!(f, "{}::", parent)?;
        }

        write!(f, "{}", self.value)?;

        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                           Value Node                                           //
////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub enum ValueNode {
    Number(Option<NumberType>, String),
    Char(String),
    String(String),
    Bool(bool),
    Type(ValueType),
    Array(Vec<PositionedNode>),
    Tuple(Vec<PositionedNode>),
    Null,
}

impl Display for ValueNode {

    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueNode::Number(number_type, value) => {
                write!(f, "{}", value)?;
                if let Some(number_type) = number_type {
                    write!(f, "{}", number_type)?;
                }
            },
            ValueNode::Char(value) => write!(f, "'{}'", value)?,
            ValueNode::String(value) => write!(f, "\"{}\"", value)?,
            ValueNode::Bool(value) => write!(f, "{}", value)?,
            ValueNode::Type(value_type) => write!(f, "{}", value_type)?,
            ValueNode::Array(values) => {
                write!(f, "{{")?;
                let mut i = 0;
                for value in values.iter() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                    i += 1;
                }
                write!(f, "}}")?;
            }
            ValueNode::Tuple(values) => {
                write!(f, "(")?;
                let mut i = 0;
                for value in values.iter() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                    i += 1;
                }
                write!(f, ")")?;
            }
            ValueNode::Null => write!(f, "null")?,
        }

        Ok(())
    }

}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                            Value Type                                          //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub type PositionedValueType = PositionedObject<ValueType>;

#[derive(Clone)]
pub enum ValueType {
    Number(NumberType),
    String,
    Char,
    Bool,
    Option(Box<PositionedValueType>),
    Reference(Box<PositionedValueType>),
    Array(Box<PositionedNode>, Box<PositionedValueType>),
    Tuple(Vec<PositionedValueType>),
    Custom(String),
}

impl Display for ValueType {

    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::Number(num_type) => write!(f, "{}", num_type)?,
            ValueType::String => write!(f, "str")?,
            ValueType::Char => write!(f, "char")?,
            ValueType::Bool => write!(f, "bool")?,
            ValueType::Option(inner) => write!(f, "?{}", inner)?,
            ValueType::Reference(inner) => write!(f, "&{}", inner)?,
            ValueType::Array(size, inner) => write!(f, "[{}]{}", size, inner)?,
            ValueType::Tuple(parts) => {
                write!(f, "(")?;
                let mut index = 0;
                for part in parts {
                    if index != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", part)?;
                    index += 1;
                }
                write!(f, ")")?;
            }
            ValueType::Custom(str) => write!(f, "{}", str)?,
        }
        Ok(())
    }

}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                            Var Type                                            //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub type PositionedVarType = PositionedObject<VarType>;

#[derive(Clone)]
pub enum VarType {
    Var,
    Let,
    Const
}

impl Display for VarType {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            VarType::Var => write!(f, "var")?,
            VarType::Let => write!(f, "let")?,
            VarType::Const => write!(f, "const")?,
        }
        Ok(())
    }
}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                          Parameter Def                                         //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub type PositionedParameterDef = PositionedObject<ParameterDef>;

#[derive(Clone)]
pub struct ParameterDef {
    pub name: PositionedString,
    pub value_type: PositionedValueType,
    pub default_value: Option<PositionedNode>,
}

impl Display for ParameterDef {

    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.name, self.value_type)?;
        if let Some(default_value) = &self.default_value {
            write!(f, "| {}", default_value)?;
        }
        Ok(())
    }

}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                         Parameter Call                                         //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub type PositionedParameterCall = PositionedObject<ParameterCall>;

#[derive(Clone)]
pub struct ParameterCall {
    pub name: Option<PositionedString>,
    pub value: PositionedNode,
}

impl Display for ParameterCall {

    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = &self.name {
            write!(f, "{} = ", name)?;
        }
        write!(f, "{}", self.value)?;
        Ok(())
    }

}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                        Condition Branch                                        //
////////////////////////////////////////////////////////////////////////////////////////////////////

#[derive(Clone)]
pub struct ConditionBranch {
    pub condition: PositionedNode,
    pub body: Vec<PositionedNode>,
    pub capture: Option<PositionedString>, // Used for option capture
}

////////////////////////////////////////////////////////////////////////////////////////////////////
//                                            Operator                                            //
////////////////////////////////////////////////////////////////////////////////////////////////////

pub type PositionedOperator = PositionedObject<Operator>;

#[derive(Clone)]
pub enum Operator {
    // Arithmetic               // Priority     Unary
    Plus,                       // 5            1
    Minus,                      // 5            1
    Multiply,                   // 4
    Divide,                     // 4
    Remainder,                  // 4
    // Bit
    BitAnd,                     // 5
    BitOr,                      // 5
    BitXor,                     // 5
    LeftShift,                  // 5
    RightShift,                 // 5
    // Bool
    And,                        // 7
    Or,                         // 7
    Xor,                        // 7
    Not,    // Only Unary       // x            1
    // Cmp
    Greater,                    // 6
    Less,                       // 6
    GreaterOrEqual,             // 6
    LessOrEqual,                // 6
    Equal,                      // 6
    NotEqual,                   // 6
    // Reference
    Ref,                        // x            1
    Deref,                      // x            1
    // Option
    Check,                      // 3
    UnwrapOr,                   // 3
    UnsafeUnwrap,               // x            3
    // Array
    Indexing,                   // 0
    SafeIndexing,               // 0
    In,                         // 6
    Range(bool),                // 6
    // Casting,
    To,                         // 2
    // Object
    Is,                         // ?
    Exact,                      // ?
    Extends                     // ?
}

impl Display for Operator {

    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Operator::Plus => write!(f, "+")?,
            Operator::Minus => write!(f, "-")?,
            Operator::Multiply => write!(f, "*")?,
            Operator::Divide => write!(f, "/")?,
            Operator::Remainder => write!(f, "%")?,
            Operator::BitAnd => write!(f, "&")?,
            Operator::BitOr => write!(f, "|")?,
            Operator::BitXor => write!(f, "^")?,
            Operator::LeftShift => write!(f, "<<")?,
            Operator::RightShift => write!(f, ">>")?,
            Operator::And => write!(f, "&&")?,
            Operator::Or => write!(f, "||")?,
            Operator::Xor => write!(f, "^^")?,
            Operator::Not => write!(f, "!")?,
            Operator::Greater => write!(f, ">")?,
            Operator::Less => write!(f, "<")?,
            Operator::GreaterOrEqual => write!(f, ">=")?,
            Operator::LessOrEqual => write!(f, "<=")?,
            Operator::Equal => write!(f, "==")?,
            Operator::NotEqual => write!(f, "!=")?,
            Operator::Ref => write!(f, "ref ")?,
            Operator::Deref => write!(f, "deref ")?,
            Operator::Check => write!(f, "?")?,
            Operator::UnwrapOr => write!(f, "unwrap_or")?,
            Operator::UnsafeUnwrap => write!(f, "unwrap ")?,
            Operator::Indexing => write!(f, "[ ]")?,
            Operator::SafeIndexing => write!(f, "?[ ]")?,
            Operator::In => write!(f, "in")?,
            Operator::Range(inclusive) => {
                if *inclusive {
                    write!(f, ":=")?;
                } else {
                    write!(f, ":")?;
                }
            }
            Operator::To => write!(f, "to")?,
            Operator::Is => write!(f, "is")?,
            Operator::Exact => write!(f, "exact")?,
            Operator::Extends => write!(f, "extends")?,
        }

        Ok(())
    }

}