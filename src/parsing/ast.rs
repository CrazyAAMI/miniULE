use std::collections::HashMap;
use crate::generating::ir::ExpressionIR;
use crate::parsing::parser::{AstParser, AstParserError};
use crate::parsing::token::Token;

#[derive(Clone, Debug)]
pub struct TokenInfo {
    first_token: Token,
    last_token: Token
}

#[derive(Clone, PartialEq, Debug)]
pub enum Operator {
    Equal,
    NotEqual,
    GreaterThan,
    GreaterEqualThan,
    LowerThan,
    LowerEqualThan,
    And,
    Or,
    Plus,
    Minus,
    Multiply,
    Divide,
}

impl Operator {
    pub fn to_str(&self) -> String {
        match &self {
            Operator::Equal => "==".to_string(),
            Operator::NotEqual => "!=".to_string(),
            Operator::GreaterThan =>  ">".to_string(),
            Operator::GreaterEqualThan =>  ">=".to_string(),
            Operator::LowerThan =>  "<".to_string(),
            Operator::LowerEqualThan =>  "<=".to_string(),
            Operator::And =>  "&&".to_string(),
            Operator::Or =>  "||".to_string(),
            Operator::Plus =>  "+".to_string(),
            Operator::Minus => "-".to_string(),
            Operator::Multiply =>  "*".to_string(),
            Operator::Divide =>  "/".to_string(),
        }
    }
}

impl HuleExpressionResultExt for Result<Operator, AstParserError> {
    fn or_reset(self, program : &mut AstParser, index : usize) -> Self
    {
        match &self {
            Ok(_) => {
                self
            }
            Err(_) => {
                program.tokens.set_current_token_index(index);
                self
            }
        }
    }
}

pub struct EvaluatedExpressionChain {

}

#[derive(Clone, PartialEq, Debug)]
pub enum ExpressionAst {
    Undefined,
    ExpressionChain(Vec<ExpressionAst>),
    ArrayDef(Vec<ExpressionAst>),
    ArrayIndex(Box<ExpressionAst>),
    StructDef(StructDefAst),
    Bracketed(Box<ExpressionAst>),
    Boolean(bool),
    String(String),
    Integer(i32),
    Identifier(String),
    FuncCall(String, Vec<ExpressionAst>),
    Binary {
        left: Box<ExpressionAst>,
        operator: Operator,
        right: Box<ExpressionAst>,
    },
}

pub trait HuleExpressionResultExt {
    fn or_reset(self, program : &mut AstParser, index : usize) -> Self;
}

impl HuleExpressionResultExt for Result<StatementAst, AstParserError> {
    fn or_reset(self, program : &mut AstParser, index : usize) -> Self
    {
        match &self {
            Ok(_) => {
                self
            }
            Err(_) => {
                program.tokens.set_current_token_index(index);
                self
            }
        }
    }
}

impl HuleExpressionResultExt for Result<ExpressionAst, AstParserError> {
    fn or_reset(self, program : &mut AstParser, index : usize) -> Self
    {
        match &self {
            Ok(_) => {
                self
            }
            Err(_) => {
                program.tokens.set_current_token_index(index);
                self
            }
        }
    }
}

impl HuleExpressionResultExt for Result<Vec<ExpressionAst>, AstParserError> {
    fn or_reset(self, program : &mut AstParser, index : usize) -> Self
    {
        match &self {
            Ok(_) => {
                self
            }
            Err(_) => {
                program.tokens.set_current_token_index(index);
                self
            }
        }
    }
}

impl HuleExpressionResultExt for Result<Token, AstParserError> {
    fn or_reset(self, program : &mut AstParser, index : usize) -> Self
    {
        match &self {
            Ok(_) => {
                self
            }
            Err(_) => {
                program.tokens.set_current_token_index(index);
                self
            }
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub enum DataTypeKind {
    Void,
    Byte,
    Integer,
    Bool,
    String,
    Struct(String)
}

impl DataTypeKind {
    pub fn from_string(str: &str) -> DataTypeKind {
        match str.to_lowercase().as_str() {
            "void" => DataTypeKind::Void,
            "byte" => DataTypeKind::Byte,
            "int" => DataTypeKind::Integer,
            "bool" => DataTypeKind::Bool,
            "string" => DataTypeKind::String,
            _ => DataTypeKind::Struct(str.to_string())
        }
    }
    pub fn to_string(&self) -> String {
        match self {
            DataTypeKind::Void => "void".to_string(),
            DataTypeKind::Byte => "byte".to_string(),
            DataTypeKind::Integer => "int".to_string(),
            DataTypeKind::Bool => "bool".to_string(),
            DataTypeKind::String => "string".to_string(),
            DataTypeKind::Struct(name) => format!("{}", name),
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct DataTypeAst {
    pub kind: DataTypeKind,
    pub is_array: bool,
    pub array_size: Option<usize>
}

impl DataTypeAst {
    pub fn default_for(kind: DataTypeKind) -> Self {
        Self {
            kind,
            is_array: false,
            array_size: None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParameterAst {
    pub data_type: DataTypeAst,
    pub name: String,
}


#[derive(Debug, Clone, PartialEq)]
pub struct FuncCallAst {
    pub name: String,
    pub parameters: Vec<ExpressionAst>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDefAst {
    pub name: String,
    pub parameters: Vec<ParameterAst>,
    pub return_type : DataTypeAst,
    pub body: BodyAst,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhileLoopAst {
    pub expression: ExpressionIR,
    pub body: BodyAst,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ForLoopMode {
    To,
    DownTo
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForLoopAst {
    pub var_name: String,
    pub start_index: ExpressionAst,
    pub end_index: ExpressionAst,
    pub body: BodyAst,
    pub mode: ForLoopMode
}

#[derive(Clone, Debug, PartialEq)]
pub struct ForInLoopAst {
    pub var_name: String,
    pub source_array_name: String,
    pub body: BodyAst,
}



#[derive(Debug, Clone, PartialEq)]
pub struct IfStatementAst {
    pub condition: ExpressionAst,
    pub body: BodyAst,
}
#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclAst {
    pub data_type: DataTypeAst,
    pub name: String,
    pub value: ExpressionAst,
}

// #[derive(Debug, Clone, PartialEq)]
// pub struct IdentifierChainKind {
//     FuncCall()
// }

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDefAst {
    pub left: ExpressionAst,
    pub value: ExpressionAst,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDeclAst {
    pub name: String,
    pub params: Vec<ParameterAst>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDefAst {
    pub name: String,
    pub values: HashMap<String, ExpressionAst>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct BodyAst {
    pub items: Vec<StatementAst>
}


impl BodyAst {
    pub fn new(items : Vec<StatementAst>) -> BodyAst {
        BodyAst {
            items
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct ProgramAst {
    pub entry: FuncDefAst,
    pub functions: Vec<FuncDefAst>,
    pub variables: Vec<VariableDeclAst>,
    pub structs: Vec<StructDeclAst>
}

#[derive(Debug, Clone, PartialEq)]
pub struct ExpressionChainAst {
    pub chain : Vec<ExpressionAst>
}

impl ExpressionChainAst {
}



/// Statements
///
#[derive(Debug, Clone, PartialEq)]
pub enum StatementAst {
    Undefined,
    ExpressionChain(ExpressionChainAst),
    VariableDecl(VariableDeclAst),
    VariableDef(VariableDefAst),
    StructDecl(StructDeclAst),
    StructDef(StructDefAst),
    IfStatement(IfStatementAst),
    WhileStatement(WhileLoopAst),
    FunctionDef(FuncDefAst),
    FunctionCall(FuncCallAst),
    Body(BodyAst),
    Return(ExpressionAst),
}

