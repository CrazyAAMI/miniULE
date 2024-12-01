use std::cell::RefCell;
use std::fmt::format;
use std::rc::Rc;
/// IR Node Types
///
/// - VarDecl / VarDef: Ule doesn't require variable declaration's but the hyperULE transpiler
///             differentiates between them during the optimization step.
use crate::generating::ir::IRNodeKind::{Block, Body, IfStatement, VarDef, WhileLoop};
use crate::parsing::ast::{DataTypeKind, ExpressionAst, FuncDefAst, Operator, ParameterAst};

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionIR {
    Identifier(String),
    ConstStr(String),
    ConstInt(i32),
    ConstArray(Vec<ExpressionIR>),
    Bracketed(Box<ExpressionIR>),
    BinaryExpression(Box<ExpressionIR>, Operator, Box<ExpressionIR>),
    FuncCallExpression(String, Vec<ExpressionIR>)
}

impl ExpressionIR {
    pub fn to_code_str(&self) -> String {
        match self {
            ExpressionIR::Identifier(identifier) => identifier.clone(),
            ExpressionIR::ConstStr(str) => str.clone(),
            ExpressionIR::ConstInt(value) => {
                format!("{}", value)
                // if *value < 255 {
                //     format!("\"{}\"", format!(r"\x{:02X}", value))
                // } else {
                //     format!("\"{}\"", format!(r"\x{:02X}\x{:02X}", (value >> 8) & 0xFF, value & 0xFF))
                // }
            },
            ExpressionIR::Bracketed(bracketed) => Self::to_code_str(bracketed),
            ExpressionIR::BinaryExpression(left, op, right) => format!("{}{}{}", left.to_code_str(), op.to_str(), right.to_code_str()),
            ExpressionIR::FuncCallExpression(_, _) => panic!("shouldnt reach this too wtf???"),

            ExpressionIR::ConstArray(array) => {
                let mut parts = vec![];
                for item in array {
                    match item {
                        ExpressionIR::ConstInt(val) => {
                            parts.push(format!("\"{}\"", format!(r"\x{:02X}", val)));
                        }
                        _ => {
                            parts.push(item.to_code_str());
                        }
                    }
                }
                parts.join("+")
            }
        }
    }

    pub fn from_ast_expression(ast_expression: &ExpressionAst) -> ExpressionIR {
        match ast_expression {
            ExpressionAst::Undefined => ExpressionIR::ConstInt(-1),
            ExpressionAst::Bracketed(bracketed) =>  ExpressionIR::Bracketed(Box::new(Self::from_ast_expression(bracketed))),
            ExpressionAst::Boolean(bool) => ExpressionIR::ConstInt(*bool as i32),
            ExpressionAst::String(str) => ExpressionIR::ConstStr(str.clone()),
            ExpressionAst::Integer(itn) => ExpressionIR::ConstInt(itn.clone()),
            ExpressionAst::Identifier(name) => ExpressionIR::Identifier(name.clone()),
            ExpressionAst::FuncCall(name, params) => ExpressionIR::FuncCallExpression(name.clone(), vec![]),
            ExpressionAst::Binary { left, operator, right }
                => ExpressionIR::BinaryExpression(
                Box::new(Self::from_ast_expression(left)),
                operator.clone(),
                Box::new(Self::from_ast_expression(right))
            ),
            ExpressionAst::ArrayDef(array) => {
                ExpressionIR::ConstStr("ARRAY OBJE".to_string())
            },
            _ => {
                panic!("not implemd yet")
            }
        }
    }

    pub fn from_ast_expressions(ast: &Vec<ExpressionAst>) -> Vec<ExpressionIR> {
        let mut result = vec![];
        for item in ast {
            result.push(ExpressionIR::from_ast_expression(item));
        }
        result
    }


    pub fn default_from_analyzer_type(data_type: &DataTypeKind) -> ExpressionIR {
        match data_type {
            DataTypeKind::Bool => ExpressionIR::ConstInt(0),
            DataTypeKind::Byte => ExpressionIR::ConstInt(0),
            DataTypeKind::Integer => ExpressionIR::ConstInt(0),
            DataTypeKind::String => ExpressionIR::ConstStr("".to_string()),
            DataTypeKind::Struct(_) => ExpressionIR::ConstInt(123),
            DataTypeKind::Void => ExpressionIR::ConstInt(0)
        }

    }
}

#[derive(Debug, Clone, PartialEq)]
enum DataFieldType {
    Integer,
    String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct DataFieldIR {
    name: String,
    data_type: DataFieldType
}

impl DataFieldIR {
    pub fn from_ast_parameter_list(params: &Vec<ParameterAst>) -> Vec<DataFieldIR> {
        let mut result = vec![];

        for param in params {
            result.push(Self::from_ast_parameter(param));
        }

        result
    }

    pub fn from_ast_parameter(param: &ParameterAst) -> DataFieldIR {
        let mut result = DataFieldIR {
            name: param.name.clone(),
            data_type: DataFieldType::Integer,
        };

        result.data_type = match &param.data_type.kind {
            DataTypeKind::Bool => DataFieldType::Integer,
            DataTypeKind::Byte => DataFieldType::Integer,
            DataTypeKind::Integer => DataFieldType::Integer,
            DataTypeKind::String => DataFieldType::String,
            DataTypeKind::Struct(name) => DataFieldType::Integer,
            _ => DataFieldType::Integer
        };



        result
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProgramIR {
    pub body: BodyIR,
}

impl ProgramIR {
    pub fn new() -> ProgramIR {
        ProgramIR {
            body: BodyIR::new(vec![])
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BodyIR {
    pub items: Vec<IRNode>
}

impl BodyIR {
    pub fn new(body : Vec<IRNode>) -> BodyIR {
        BodyIR {
            items: body
        }
    }
}


#[derive(Debug, Clone, PartialEq)]
pub struct IfStatementIR {
    pub condition: ExpressionIR,
    pub then_body: BodyIR,
    pub else_body: Option<BodyIR>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct WhileLoopIR {
    while_condition: ExpressionIR,
    body: BodyIR,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDefIR {
    pub name: String,
    pub value: ExpressionIR,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IRNodeKind {
    Block(BodyIR),
    VarDef(VarDefIR),
    WhileLoop(WhileLoopIR),
    IfStatement(IfStatementIR),
    Expression(ExpressionIR),
    Body(BodyIR),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IRNode {
    pub kind: IRNodeKind
}

impl IRNode {
    pub fn block(block: BodyIR) -> IRNode {
        IRNode {
            kind: Block(block)
        }
    }

    pub fn var_def(var_def: VarDefIR) -> IRNode {
        IRNode {
            kind: VarDef(var_def)
        }
    }

    pub fn while_loop(while_loop: WhileLoopIR) -> IRNode {
        IRNode {
            kind: WhileLoop(while_loop)
        }
    }

    pub fn if_statement(if_statement: IfStatementIR) -> IRNode {
        IRNode {
            kind: IfStatement(if_statement)
        }
    }

    pub fn body(body: BodyIR) -> IRNode {
        IRNode {
            kind: Body(body)
        }
    }
}
