use crate::parsing::static_analyzer::AnalyzerErrorKind::*;
use crate::parsing::ast::{BodyAst, DataTypeAst, DataTypeKind, ExpressionAst, FuncCallAst, FuncDefAst, IfStatementAst, Operator, ParameterAst, ProgramAst, StatementAst, StructDeclAst, StructDefAst, VariableDeclAst, VariableDefAst};
use crate::parsing::ast::*;
use crate::parsing::ast::DataTypeKind::Void;


/// Enum representing the type of the analyzer error.
#[derive(Clone, Debug, PartialEq)]
pub enum AnalyzerErrorKind {
    Undefined,
    UnknownType,
    IllegalStatement(StatementAst),
    IllegalOperator(Operator),
    RedefinedVarName(String),
    UnexpectedStatement(StatementAst),
    TypeMismatch(String, String), // Expected Given
    IllegalTypeUsage(String),
    ParameterExpected(String),
    RecursiveFunctionFound(String, Vec<String>),
    ParameterCountWrong(usize),
    FunctionNotFound(String),
    LeftSideNotWritable(ExpressionAst),
}
impl AnalyzerTypeKind {

    pub fn to_message(&self) -> String {
        match &self {
            AnalyzerErrorKind::TypeMismatch(expected, given) => {
                format!("Type mismatch: {} but {} given.", expected, given)
            }
            AnalyzerErrorKind::UnexpectedStatement(statement) => {
                format!("Unexpected statement: {:?}", statement)
            }
            _ => {
                format!("Undefined error during analyzing.")
            }
        }
    }
    
    pub fn undefined(expected: &str, given: &str) -> AnalyzerErrorKind {
        AnalyzerErrorKind::Undefined
    }

    pub fn expression_not_writable(expr: &ExpressionAst)  -> AnalyzerErrorKind {
        AnalyzerErrorKind::LeftSideNotWritable(expr.clone())
    }

    pub fn recursive_function_found(name: &str, stack: Vec<String>) -> AnalyzerErrorKind {
        AnalyzerErrorKind::RecursiveFunctionFound(name.to_string(), stack)
    }

    pub fn parameter_expected(param_name: &str) -> AnalyzerErrorKind {
        AnalyzerErrorKind::ParameterExpected(param_name.to_string())
    }

    pub fn function_not_found(func_name: &str) -> AnalyzerErrorKind {
        AnalyzerErrorKind::FunctionNotFound(func_name.to_string())
    }

    pub fn parameter_count_wrong(param_count: usize) -> AnalyzerErrorKind {
        AnalyzerErrorKind::ParameterCountWrong(param_count)
    }

    pub fn redefined_var_name(var_name: &str) -> AnalyzerErrorKind {
        AnalyzerErrorKind::RedefinedVarName(var_name.to_string())
    }

    pub fn illegal_type_usage(description : &str) -> AnalyzerErrorKind {
        AnalyzerErrorKind::IllegalTypeUsage(description.to_string())
    }

    pub fn illegal_operator(operator: Operator) -> AnalyzerErrorKind {
        AnalyzerErrorKind::IllegalOperator(operator)
    }

    pub fn illegal_statement(operator: StatementAst) -> AnalyzerErrorKind {
        AnalyzerErrorKind::IllegalStatement(operator)
    }

    pub fn unknown_type() -> AnalyzerErrorKind {
         AnalyzerErrorKind::UnknownType
    }

    pub fn type_mismatch(expected: &str, given: &str) -> AnalyzerErrorKind {
        TypeMismatch(expected.to_string(), given.to_string())
    }

    pub fn unexpected_statement(statement: StatementAst) -> AnalyzerErrorKind {
        AnalyzerErrorKind::UnexpectedStatement(statement)
    }

}

#[derive(Debug)]
struct AnalyzerScope {
    scopes: Vec<i32>
}

#[derive(Debug)]
struct AnalyzerFunction {
    name: String,
    parameters: Vec<ParameterAst>,
    scope: AnalyzerScope
}

#[derive(Clone, Debug, PartialEq)]
pub enum AnalyzerTypeKind {
    Boolean,
    Byte,
    Integer,
    String,
    Struct
}

impl AnalyzerTypeKind {
    pub fn as_str(&self) -> String {
        match self {
            AnalyzerTypeKind::Boolean => "Boolean".to_string(),
            AnalyzerTypeKind::Byte => "Byte".to_string(),
            AnalyzerTypeKind::Integer => "Integer".to_string(),
            AnalyzerTypeKind::String => "String".to_string(),
            AnalyzerTypeKind::Struct => "Struct".to_string(),
        }
    }
}



#[derive(Clone, Debug, PartialEq)]
pub struct AnalyzerFuncData {
    pub ast: FuncDefAst,
    usages: usize,
    recursive: bool,
    size: usize,
    complex_parameters: bool
}

impl AnalyzerFuncData {
    pub fn new(func_def: FuncDefAst) -> AnalyzerFuncData {
        AnalyzerFuncData {
            ast: func_def,
            usages: 0,
            recursive: false,
            size: 0,
            complex_parameters: false,
        }
    }
}



#[derive(Clone, Debug)]
pub struct AnalyzerInfo {
    current_parameters: Vec<ParameterAst>,

    pub vars : Vec<VariableDeclAst>,
    pub structs : Vec<StructDeclAst>,
    pub functions: Vec<AnalyzerFuncData>,
    pub local_vars : Vec<VariableDeclAst>,
}

pub enum AnalyzerChainKind {
    Identifier,
    ArraySelector,
    Call,
}

pub struct AnalyzerChainResult {
    pub valid: bool,
    pub end_kind: AnalyzerChainKind
}

impl AnalyzerInfo {
    // pub fn find_var(&self, name: &str) -> Option<&VariableDeclAst> {
    //     self.local_vars.iter().find(|i| i.name == name)
    //         .or_else(|| self.vars.iter().find(|i| i.name == name))
    // }

    pub fn find_var(&self, name: &str) -> Option<&VariableDeclAst> {
        self.local_vars.iter().find(|i| i.name == name)
            .or_else(|| self.vars.iter().find(|i| i.name == name))
    }

    pub fn is_valid_chain(&self, chain: &ExpressionChainAst) -> Result<AnalyzerChainResult, AnalyzerErrorKind> {
        let mut index = 0;
        let mut last_type : DataTypeAst;

        if let Ok(ExpressionAst::Identifier(name)) = &chain.chain.first() {
            if let Some(var_decl) = self.find_var(&name) {
                last_type = var_decl.data_type.clone();
            } else {
                return Err(AnalyzerErrorKind::Undefined);
            }
        } else {
            Err(AnalyzerErrorKind::UnexpectedStatement(StatementAst::ExpressionChain(chain.clone())))?;
        }

        if index > 1 {
            for c in &1..&chain.chain.len() {

            }
        }

        for item in &chain.chain {
            if index == 0 {
                if let ExpressionAst::Identifier(name) = item {
                    if let Some(var_decl) = self.find_var(&name) {
                        last_type = var_decl.data_type.clone();
                    } else {
                        return Err(AnalyzerErrorKind::Undefined);
                    }
                }
            } else {
                match item {
                    ExpressionAst::ArrayIndex(_) => {}
                    ExpressionAst::Identifier(_) => {
                        if let DataTypeKind::Struct(name) = last_type.kind {

                        }
                    }
                    ExpressionAst::FuncCall(_, _) => {}
                    _ => return Err(AnalyzerErrorKind::undefined(&"i dont know", &"wtf"))
                }
            }
            index += 1;
        }

        if let Ok(last) = chain.chain.last() {
            Ok(true)
        } else {
            Ok(false)
        }
    }
}


#[derive(Clone, Debug)]
pub struct Analyzer {
    program : ProgramAst
}

impl Analyzer {

    fn do_types_match(info: &mut AnalyzerInfo, expr1: &ExpressionAst, expr2: &ExpressionAst) -> Result<bool, AnalyzerErrorKind> {
        Ok(Analyzer::evaluate_expression_type(info, expr1)? == Analyzer::evaluate_expression_type(info, expr2)?)
    }


    fn evaluate_binary_expression_type(info: &AnalyzerInfo, left: &ExpressionAst, operator: &Operator, right: &ExpressionAst) -> Result<DataTypeAst, AnalyzerErrorKind> {
        let left_type = Self::evaluate_expression_type(info, left)?;
        let right_type = Self::evaluate_expression_type(info, right)?;

        if left_type != right_type {
            return Err(AnalyzerErrorKind::TypeMismatch(left_type.kind.to_string(), right_type.kind.to_string()));
        }

        let mut ret_type = left_type.clone();
        match operator {
            Operator::Equal |
            Operator::NotEqual |
            Operator::GreaterThan |
            Operator::GreaterEqualThan |
            Operator::LowerThan |
            Operator::LowerEqualThan |
            Operator::And |
            Operator::Or => {
                ret_type.kind = DataTypeKind::Bool;
            }
            Operator::Plus => {
                ret_type.kind = left_type.kind;
            }
            Operator::Minus |
            Operator::Multiply |
            Operator::Divide => {
                ret_type.kind = DataTypeKind::Integer;
            }
        }

        Ok(ret_type)
    }

    fn evaluate_expression_type(info: &AnalyzerInfo, expr: &ExpressionAst) -> Result<DataTypeAst, AnalyzerErrorKind> {
        match expr {
            ExpressionAst::Undefined => {
                panic!("Undefined implemented yet");
            }
            ExpressionAst::Bracketed(bracketed) => {
                Ok(Self::evaluate_expression_type(info, bracketed)?)
            }
            ExpressionAst::Boolean(_) => {
                Ok(DataTypeAst::default_for(DataTypeKind::Bool))
            }
            ExpressionAst::String(_) => {
                Ok(DataTypeAst::default_for(DataTypeKind::String))
            }
            ExpressionAst::Integer(_) => {
                Ok(DataTypeAst::default_for(DataTypeKind::Integer))
            }
            ExpressionAst::Identifier(identifier) => {
                Err(AnalyzerErrorKind::unknown_type())
            }
            ExpressionAst::FuncCall(_, _) => {
                panic!("Call not implemented yet");
            }
            ExpressionAst::Binary { left, operator, right } => {
                Ok(Self::evaluate_binary_expression_type(info, left, operator, right)?)
            }
            ExpressionAst::ArrayDef(array) => {
                let mut kind = DataTypeAst::default_for(Void);
                let mut index = 0;

                for item in array {
                    if index == 0 {
                        kind = Self::evaluate_expression_type(&info, &item)?;
                    } else {
                        let given = Self::evaluate_expression_type(&info, &item)?;
                        if kind != given {
                            return Err(AnalyzerErrorKind::type_mismatch(&kind.kind.to_string(), &given.kind.to_string()));
                        }
                    }
                    index += 1;
                }

                if index > 0 {
                    kind.array_size = Some(index)
                }

                Ok(kind)
            }
            _ => {
                panic!("not implemd yet")
            }
        }
    }

    fn handle_local_var_decl(info: &mut AnalyzerInfo, var_decl: &VariableDeclAst) -> Result<(), AnalyzerErrorKind> {
        if info.find_var(&var_decl.name).is_none() {
            info.local_vars.push(var_decl.clone());
            println!("declared {} as {:?}", var_decl.name, var_decl.data_type);

            Self::analyze_var_def(info, &VariableDefAst {
                left: ExpressionAst::Identifier(var_decl.name.clone()),
                value: var_decl.value.clone(),
            })?;
        } else {
            Err(AnalyzerErrorKind::redefined_var_name(var_decl.name.as_str()))?;
        }
        Ok(())
    }

    fn analyze_global_var_decl(info: &mut AnalyzerInfo, var_decl: &VariableDeclAst) -> Result<(), AnalyzerErrorKind> {
        Self::analyze_var_def(info, &VariableDefAst {
            left: ExpressionAst::Identifier(var_decl.name.clone()),
            value: var_decl.value.clone(),
        })?;
        Ok(())
    }

    fn is_expression_writable(info: &mut AnalyzerInfo, expr: &ExpressionAst) -> bool {
        if let ExpressionAst::ExpressionChain(chain) = expr {
            let mut result = true;

            let mut c = 0;
            for item in chain {
                if c == 0 { // todo: as of first element MUST be an identifer
                    match item {
                        ExpressionAst::Identifier(_) => result = true,
                        _ => {
                            return false;
                        }
                    }
                } else {
                    match item {
                        ExpressionAst::Identifier(_) => result = true,
                        ExpressionAst::FuncCall(..) => return false,
                        ExpressionAst::ArrayIndex(..) => result = true,
                        _ => {
                            return false;
                        }
                    }
                }
                c += 1;
            }

            result
        } else {
            match expr {
                ExpressionAst::Identifier(_) => {
                    true
                }
                _ => false
            }
        }
    }

    fn analyze_var_def(info: &mut AnalyzerInfo, var_def: &VariableDefAst) -> Result<(), AnalyzerErrorKind> {
        if !Self::is_expression_writable(info, &var_def.left) {
            return Err(AnalyzerErrorKind::expression_not_writable(&var_def.left));
        }


        // if let Some(var_decl) = info.find_var(&var_def.name) {
        //     println!("defining {} as {:?}", var_def.name, var_def.value);
        //     let defined_var_type = Self::evaluate_expression_type(info, &var_def.value)?;
        //
        //     if var_decl.data_type.kind == defined_var_type.kind {
        //         println!("type is matching ({:?} & {:?})", &var_decl.data_type.kind, &defined_var_type.kind);
        //     } else {
        //         println!("ERRPR: type is not matching");
        //         Err(AnalyzerErrorKind::type_mismatch(&var_decl.data_type.kind.to_string(), &defined_var_type.kind.to_string()))?
        //     }
        // } else {
        //     Err(AnalyzerErrorKind::unknown_type())?
        // }


        Ok(())
    }

    fn handle_if_statement(info: &mut AnalyzerInfo, if_statement: &IfStatementAst) -> Result<(), AnalyzerErrorKind> {
        let evaluated_condition = Self::evaluate_expression_type(info, &if_statement.condition)?;
        if evaluated_condition.kind != DataTypeKind::Bool {
            Err(AnalyzerErrorKind::type_mismatch("Boolean", &evaluated_condition.kind.to_string()))
        } else {
            Ok(())
        }
    }

    fn handle_func_call(info: &mut AnalyzerInfo, func_call: &FuncCallAst) -> Result<(), AnalyzerErrorKind> {
        let cloned = info.clone();
        if let Some(called_func) = info.functions.iter_mut().find(|i| i.ast.name == func_call.name) {
            let mut index = 0;
            if called_func.ast.parameters.len().clone() != func_call.parameters.len() {
                return Err(AnalyzerErrorKind::parameter_count_wrong(called_func.ast.parameters.len()))
            }

            for param in &called_func.ast.parameters {
                let passed_parameter = func_call.parameters.get(index as usize);

                if let Some(passed_parameter) = passed_parameter {
                    if let Ok(evaluated) = Self::evaluate_expression_type(&cloned, passed_parameter) {
                        if evaluated != param.data_type.clone() {
                            return Err(AnalyzerErrorKind::parameter_expected(&param.name))
                        }
                    } else {
                        panic!("could not eval expression")
                    }
                } else {
                    return Err(AnalyzerErrorKind::parameter_expected(&param.name))
                }

                index += 1;
            }

            called_func.usages += 1;
        } else {
            return Err(AnalyzerErrorKind::function_not_found(&func_call.name))
        }
        Ok(())
    }

    fn handle_return(info: &mut AnalyzerInfo, expr: &ExpressionAst) -> Result<(), AnalyzerErrorKind> {
        Ok(())
    }

    fn analyze_local_body(info: &mut AnalyzerInfo, body: &BodyAst) -> Result<(), AnalyzerErrorKind> {
        let var_index = info.local_vars.len();

        for item in body.items.clone() {
            match item {
                StatementAst::VariableDecl(var_decl) => {
                    Self::handle_local_var_decl(info, &var_decl)?;
                }
                StatementAst::VariableDef(var_def) => {
                    Self::analyze_var_def(info, &var_def)?;
                }
                StatementAst::IfStatement(if_statement) => {
                    Self::handle_if_statement(info, &if_statement)?;
                }
                StatementAst::FunctionCall(func_call) => {
                    Self::handle_func_call(info, &func_call)?;
                }
                StatementAst::Return(expr) => {
                    Self::handle_return(info, &expr)?;
                }
                _ => {
                    return Err(AnalyzerErrorKind::unexpected_statement(item));
                }
            }
        }

        info.local_vars.truncate(var_index);
        Ok(())
    }

    fn analyze_func(info: &mut AnalyzerInfo, func_decl: &FuncDefAst) -> Result<(), AnalyzerErrorKind> {
        println!("analyzing function {}...", func_decl.name);

        info.current_parameters = func_decl.parameters.clone();

        info.local_vars.clear(); // clear local vars for this new function
        Self::analyze_local_body(info, &func_decl.body)
    }

    fn analyze_recursive_func_call(info: &mut AnalyzerInfo, func: &FuncDefAst, stack: &mut Vec<String>) -> Result<(), AnalyzerErrorKind> {
        if stack.contains(&func.name) {
            return Err(AnalyzerErrorKind::recursive_function_found(&func.name, stack.clone()));
        }

        stack.push(func.name.clone());

        for item in func.body.items.clone() {
            match item {
                StatementAst::FunctionCall(func_call) => {
                    let functions = info.functions.clone();
                    let func_def = functions.iter().find(|f| f.ast.name == func_call.name);

                    if let Some(func_def) = func_def {
                        Self::analyze_recursive_func_call(info, &func_def.ast, stack)?
                    }
                }
                _ => {}
            }
        }
        stack.pop();
        Ok(())
    }

    pub fn analyze_recursive_funcs(info: &mut AnalyzerInfo, entry: &FuncDefAst) -> Result<(), AnalyzerErrorKind> {
        let mut stack = vec![];

        Self::analyze_recursive_func_call(info, &entry, &mut stack)?;

        Ok(())
    }

    pub fn analyze(ast: ProgramAst) -> Result<AnalyzerInfo, AnalyzerErrorKind> {
        let mut info = Self::build_struct(&ast)?;

        for var in info.vars.clone() {
            Self::analyze_global_var_decl(&mut info, &var)?;
        }

        Self::analyze_func(&mut info, &ast.entry)?;
        for func in info.functions.clone() {
            Self::analyze_func(&mut info, &func.ast)?;
        }

        Self::analyze_recursive_funcs(&mut info, &ast.entry)?;

        Ok(info)
    }


    fn build_struct(ast: &ProgramAst) -> Result<AnalyzerInfo, AnalyzerErrorKind> {
        let mut functions = vec![];
        for item in ast.functions.clone() {
            functions.push(AnalyzerFuncData::new(item));
        }

        Ok(AnalyzerInfo {
            current_parameters: vec![],
            vars: ast.variables.clone(),
            structs: vec![],
            functions,
            local_vars: vec![],
        })
    }
}