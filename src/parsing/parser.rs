use std::string::String;
use std::cmp::PartialEq;
use std::collections::HashMap;
use std::string::ParseError;
use crate::generating::ir::ExpressionIR;
use crate::parsing::ast::{BodyAst, DataTypeAst, DataTypeKind, ExpressionAst, ExpressionChainAst, ForLoopAst, ForLoopMode, FuncCallAst, FuncDefAst, HuleExpressionResultExt, IfStatementAst, Operator, ParameterAst, ProgramAst, StatementAst, StructDeclAst, StructDefAst, VariableDeclAst, VariableDefAst};
use crate::parsing::ast::DataTypeKind::Void;
use crate::parsing::ast::StatementAst::Body;
use crate::parsing::parser::AstParserErrorKind::{Custom, EndOfFile, IncompatibleStatement, SomeTokenExpected, StatementExpected, TokenExpected, UnexpectedEof};
use crate::parsing::token::{Token, TokenType};
use crate::parsing::tokenizer::{Tokenized, Tokenizer};

#[derive(Debug, Clone, PartialEq)]
pub enum AstParserErrorKind {
    EndOfFile,
    UnexpectedEof,

    TokenExpected(String, String),
    SomeTokenExpected(Vec<TokenType>, String),
    StatementExpected(String),

    IncompatibleStatement,
    Custom(String)
}

#[derive(Debug, Clone, PartialEq)]
pub struct AstParserError {
    kind: AstParserErrorKind,
    pub(crate) token_position: usize
}

impl AstParserError {
    pub fn new(kind : AstParserErrorKind, token_position: usize) -> AstParserError {
        AstParserError {
            kind,
            token_position
        }
    }

    pub fn end_of_file(position_token: usize) -> AstParserError {
        Self::new(EndOfFile, position_token)
    }

    pub fn unexpected_end_of_file(position_token: usize) -> AstParserError {
        Self::new(UnexpectedEof, position_token)
    }

    pub fn token_expected(expected: String, given: String, position_token: usize) -> AstParserError {
        Self::new(TokenExpected(expected, given), position_token)
    }

    pub fn custom(msg: String, position_token: usize) -> AstParserError {
        Self::new(Custom(msg), position_token)
    }

    pub fn some_token_expected(expected: Vec<TokenType>, given: String, position_token: usize) -> AstParserError {
        Self::new(SomeTokenExpected(expected, given.to_string()), position_token)
    }

    pub fn statement_expected(expected: &str, position_token: usize) -> AstParserError {
        Self::new(StatementExpected(expected.to_string()), position_token)
    }

    pub fn incompatible_statement() -> AstParserError {
        Self::new(IncompatibleStatement, 0)
    }

    pub fn to_message(&self) -> String {
        match &self.kind {
            AstParserErrorKind::EndOfFile => format!("EndOfFile"),
            AstParserErrorKind::UnexpectedEof => "UnexpectedEof".to_string(),
            AstParserErrorKind::TokenExpected(expected, given)
            => format!("Token '{}' expected but '{}' given.", expected, given),
            AstParserErrorKind::SomeTokenExpected(_, _) => "SomeTokenExpected".to_string(),
            AstParserErrorKind::StatementExpected(_) => "StatementExpected".to_string(),
            AstParserErrorKind::IncompatibleStatement => "IncompatibleStatement".to_string(),
            AstParserErrorKind::Custom(msg)
            => format!("Custom: {}", msg),
        }
    }
}

#[derive(Clone)]
pub struct AstParser {
    pub tokens : Tokenized,

}


impl AstParser {
    fn expect_token_type(&mut self, token_type: TokenType) -> Result<Token, AstParserError> {
        let position = self.tokens.position().clone();
        let mut token = self.tokens.next().ok_or_else(|| AstParserError::unexpected_end_of_file(position))?.clone();
        if token.get_token_type() != token_type {
            self.tokens.prev();
            return Err(AstParserError::token_expected(format!("{}", token_type.to_string()), token.value, position));
        }

        Ok(token)
    }
    fn expect_some_token(&mut self, tokens: Vec<TokenType>) -> Result<Token, AstParserError> {
        let position = self.tokens.position().clone();
        let mut token = self.tokens.next().ok_or_else(|| AstParserError::unexpected_end_of_file(position))?.clone();
        if tokens.contains(&token.get_calculated_token_type()) {
            self.tokens.prev();
            return Err(AstParserError::some_token_expected(tokens, format!("{:?}", token.value), position));
        }

        Ok(token)
    }

    fn expect_token_value(&mut self, token_value: &str) -> Result<Token, AstParserError> {
        let position = self.tokens.position().clone();
        let mut token = self.tokens.next().ok_or_else(|| AstParserError::unexpected_end_of_file(position))?.clone();
        if token.value != token_value {
            self.tokens.prev();
            return Err(AstParserError::token_expected(format!("{}", token_value), token.value, position));
        }

        Ok(token)
    }

    fn expect_some_token_value(&mut self, token_values: Vec<String>) -> Result<Token, AstParserError> {
        let position = self.tokens.position().clone();
        let mut token = self.tokens.next().ok_or_else(|| AstParserError::unexpected_end_of_file(position))?.clone();
        if token_values.contains(&token.value) {
            self.tokens.prev();
            return Err(AstParserError::token_expected(format!("{:?}", token_values), token.value, position));
        }

        Ok(token)
    }

    fn try_parse_if_statement(&mut self) -> Result<StatementAst, AstParserError> {
        let _ = self.expect_token_value("if")
            .or_else(|_| Err(AstParserError::incompatible_statement()))?;

        self.expect_token_type(TokenType::BracketOpen)?;

        let condition = self.try_parse_expression()?;

        self.expect_token_type(TokenType::BracketClose)?;
        self.expect_token_type(TokenType::CurlyBracketOpen)?;

        let mut hule_body = BodyAst::new(vec![]);
        let parsed_body = self.try_parse_local_body();
        if let Err(err) = &parsed_body {
            if *err != AstParserError::incompatible_statement() {
                return Err(err.clone());
            }
        } else if let Body(body) = &parsed_body.unwrap() {
            hule_body = body.clone();
        }

        self.expect_token_type(TokenType::CurlyBracketClose)?;
        Ok(StatementAst::IfStatement(IfStatementAst {
            condition,
            body: hule_body
        }))
    }

    fn try_parse_bracket_expression(&mut self) -> Result<ExpressionAst, AstParserError> {
        let start_index = self.tokens.get_current_token_index();

        self.expect_token_type(TokenType::BracketOpen)
            .or_reset(self, start_index)
            .or_else(|_| Err(AstParserError::incompatible_statement()))?;

        let result = self.try_parse_expression()
            .or_else(|_| Err(AstParserError::incompatible_statement()))?;

        self.expect_token_type(TokenType::BracketClose)?;

        Ok(result)
    }

    fn try_parse_simple_expression(&mut self) -> Result<ExpressionAst, AstParserError> {
        let current_index = self.tokens.get_current_token_index();

        if let Some(current_token) = self.tokens.next() {
            match current_token.get_calculated_token_type() {
                TokenType::Identifier => Ok(ExpressionAst::Identifier(current_token.value.clone())),
                TokenType::ConstStringExpression => Ok(ExpressionAst::String(current_token.value.clone())),
                TokenType::ConstIntegerExpression => Ok(ExpressionAst::Integer(current_token.value.parse().unwrap())),
                _ => {
                    self.tokens.set_current_token_index(current_index);
                    Err(AstParserError::incompatible_statement())
                },
            }
        } else {
            self.tokens.set_current_token_index(current_index);
            Err(AstParserError::incompatible_statement())
        }
    }

    fn try_parse_struct_expression(&mut self) -> Result<ExpressionAst, AstParserError> {
        let struct_name = self.expect_token_type(TokenType::Identifier)
            .or_else(|_| Err(AstParserError::incompatible_statement()))?;

        self.expect_token_type(TokenType::CurlyBracketOpen)?;
        let mut values = HashMap::new();

        loop {
            let member_name = self.expect_token_type(TokenType::Identifier)?;
            self.expect_token_type(TokenType::Assign)?;
            let member_value = self.try_parse_expression()?;

            let token = self.expect_token_type(TokenType::CurlyBracketClose)
                .or_else(|_| self.expect_token_type(TokenType::CurlyBracketClose))?;

            values.insert(member_name.value, member_value);

            if token.get_calculated_token_type() == TokenType::CurlyBracketClose {
                break;
            }
        }

        Ok(ExpressionAst::StructDef(StructDefAst {
            name: struct_name.value,
            values,
        }))
    }

    fn try_parse_array_expression(&mut self) -> Result<ExpressionAst, AstParserError> {
        let start_index = self.tokens.get_current_token_index();
        let mut items = vec![];

        self.expect_token_type(TokenType::SquareBracketOpen).or_else(|_| Err(AstParserError::incompatible_statement()))?;

        loop {
            let current_index = self.tokens.get_current_token_index();
            let expr = self.try_parse_expression();

            if let Ok(expr) = expr {
                items.push(expr);
            }

            let token = self.expect_token_type(TokenType::Comma)
                .or_else(|_| self.expect_token_type(TokenType::SquareBracketClose))?;

            if token.get_calculated_token_type() == TokenType::SquareBracketClose {
                break;
            }
        }

        Ok(ExpressionAst::ArrayDef(items))
    }

    fn try_parse_binary_operator(&mut self) -> Result<Operator, AstParserError> {
        let current_index = self.tokens.get_current_token_index();

        if let Some(current_token) = self.tokens.next() {
            match current_token.get_calculated_token_type() {
                TokenType::Equal => return Ok(Operator::Equal),
                TokenType::NotEqual => return Ok(Operator::NotEqual),
                TokenType::GreaterThan => return Ok(Operator::GreaterThan),
                TokenType::LowerThan => return Ok(Operator::LowerThan),
                TokenType::GreaterEqualThan => return Ok(Operator::GreaterEqualThan),
                TokenType::LowerEqualThan => return Ok(Operator::LowerEqualThan),
                TokenType::And => return Ok(Operator::And),
                TokenType::Or => return Ok(Operator::Or),
                TokenType::Plus => return Ok(Operator::Plus),
                TokenType::Minus => return Ok(Operator::Minus),
                TokenType::Divide => return Ok(Operator::Divide),
                TokenType::Multiply => return Ok(Operator::Multiply),
                _ => return Err(AstParserError::incompatible_statement())
            }
        } else {
            self.tokens.set_current_token_index(current_index);
            Err(AstParserError::incompatible_statement())
        }
    }

    fn try_parse_binary_expression(&mut self, left_side: ExpressionAst) -> Result<ExpressionAst, AstParserError>  {
        let current_index = self.tokens.get_current_token_index();

        let operator = self.try_parse_binary_operator()
            .or_reset(self, current_index)?;

        let right_side = self.try_parse_expression()
            .or_reset(self, current_index)?;

        Ok(ExpressionAst::Binary {
            left: Box::new(left_side),
            right: Box::new(right_side),
            operator: operator.clone(),
        })
    }

    fn try_parse_identifier(&mut self) -> Result<ExpressionAst, AstParserError> {
        if let Ok(name) = self.expect_token_type(TokenType::Identifier) {
            Ok(ExpressionAst::Identifier(name.value))
        } else {
            Err(AstParserError::incompatible_statement())
        }
    }

    fn try_parse_chained_expression(&mut self, input: ExpressionAst)  -> Result<ExpressionAst, AstParserError> {
        let mut items = vec![input];

        loop {
            let start_index = self.tokens.get_current_token_index();


            if self.expect_token_type(TokenType::SquareBracketOpen).is_ok() {// check if array
                if let Ok(expr) = self.try_parse_expression() {
                    self.expect_token_type(TokenType::SquareBracketClose)?;
                    items.push(ExpressionAst::ArrayIndex(Box::new(expr)));
                } else {
                    self.tokens.set_current_token_index(start_index);
                    break;
                }
            } else if self.expect_token_type(TokenType::Dot).is_ok() { // check if another chain follows
                let current_index = self.tokens.get_current_token_index();
                let expr = self.try_parse_func_call_as_expression().or_reset(self, current_index)
                    .or_else(|_| self.try_parse_identifier()).or_reset(self, current_index);

                if let Ok(expr) = expr {
                    items.push(expr);
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        if items.len() == 1 {
            Ok(items[0].clone())
        } else {
            Ok(ExpressionAst::ExpressionChain(items.clone()))
        }
    }

    fn try_parse_expression(&mut self) -> Result<ExpressionAst, AstParserError> {
        let mut result = ExpressionAst::Undefined;

        let bracket = self.expect_token_type(TokenType::BracketOpen);

        let mut current_index = self.tokens.get_current_token_index();

        if let Ok(expr) = self.try_parse_func_call_as_expression().or_reset(self, current_index)
            .or_else(|_| self.try_parse_struct_expression()).or_reset(self, current_index)
            .or_else(|_| self.try_parse_array_expression()).or_reset(self, current_index)
            .or_else(|_| self.try_parse_simple_expression()).or_reset(self, current_index) {
            current_index = self.tokens.get_current_token_index();

            let binary_expression = self.try_parse_binary_expression(expr.clone())
                .or_reset(self, current_index)
                .unwrap_or(ExpressionAst::Undefined);

            if binary_expression == ExpressionAst::Undefined {
                result = expr;
            } else {
                result = binary_expression;
            }
        }

        let parsed_result = result.clone();
        if bracket.is_ok() {
            //bracket
            result = ExpressionAst::Bracketed(Box::new(parsed_result));

            self.expect_token_type(TokenType::BracketClose)?;
        }

        let chained = self.try_parse_chained_expression(result.clone());
        if let Ok(ExpressionAst::ExpressionChain(chain)) = chained  {
            if let Some(last) = chain.last() {

            }
            result = ExpressionAst::ExpressionChain(chain);
        }



        Ok(result)
    }

    fn try_parse_type(&mut self) -> Result<DataTypeAst, AstParserError> {
        let mut is_array = false;

        let is_ref = self.expect_token_type(TokenType::And).is_ok();

        let identifier = self.expect_token_type(TokenType::Identifier)
            .map_err(|_| AstParserError::incompatible_statement())?;

        let mut array_size = None;
        if self.expect_token_type(TokenType::SquareBracketOpen).is_ok() {
            let size = self.expect_token_type(TokenType::ConstIntegerExpression);

            if let Ok(size) = size {
                let size = size.value.parse::<usize>();
                if let Ok(size) = size {
                    array_size = Some(size);
                } else {
                    return Err(AstParserError::custom("size must be valid usize".to_string(), 0));
                }
            }

            self.expect_token_type(TokenType::SquareBracketClose)?;
            is_array = true;
        }

        Ok(DataTypeAst {
            kind: DataTypeKind::from_string(&identifier.value),
            is_array,
            array_size,
        })
    }

    fn try_parse_var_decl(&mut self) -> Result<StatementAst, AstParserError> {
        let mut var_type = self.try_parse_type()?;

        let var_name = self.expect_token_type(TokenType::Identifier)?;
        //let left = self.try_parse_expression()?;

        self.expect_token_type(TokenType::Assign)?;

        let var_value = self.try_parse_expression()?;
        self.expect_token_type(TokenType::Semicolon)?;

        Ok(StatementAst::VariableDecl(VariableDeclAst {
            data_type: var_type,
            name: var_name.value,
            value: var_value
        }))
    }

    fn try_parse_var_def(&mut self) -> Result<StatementAst, AstParserError> {
        //let var_name = self.expect_token_type(TokenType::Identifier)
        let left = self.try_parse_expression()
            .map_err(|_| AstParserError::incompatible_statement())?;

        self.expect_token_type(TokenType::Assign)?;

        let var_value = self.try_parse_expression()?;

        self.expect_token_type(TokenType::Semicolon)?;

        Ok(StatementAst::VariableDef(VariableDefAst {
            left,
            value: var_value
        }))
    }

    fn try_parse_statement(&mut self) -> Result<StatementAst, AstParserError> {
        let index = self.tokens.get_current_token_index();
        let mut statement = self.try_parse_var_decl().or_reset(self, index);

        if statement.is_err() {
            self.tokens.set_current_token_index(index);
            statement = self.try_parse_func_call().or_reset(self, index);
        }

        if let Ok(StatementAst::FunctionCall(sta)) = &statement {
            println!("FOUND CALL");
        }

        statement
    }

    fn try_parse_local_body(&mut self) -> Result<StatementAst, AstParserError> {
        let mut result : Vec<StatementAst> = vec![];
        let mut last_error = AstParserError::incompatible_statement();

        loop {
            let current_pos = self.tokens.get_current_token_index();


            let mut parsed = self.try_parse_var_decl().or_reset(self, current_pos)
                .or_else(|_| self.try_parse_var_def()).or_reset(self, current_pos)
                .or_else(|_| self.try_parse_if_statement()).or_reset(self, current_pos)
                .or_else(|_| self.try_parse_func_call()).or_reset(self, current_pos);
            // let mut parsed = self.try_parse_statement().or_reset(self, current_pos);
            // // let mut parsed = self.try_parse_var_decl().or_reset(self, current_pos)
            // //     .or_else(|_| self.try_parse_var_def()).or_reset(self, current_pos)
            // //     .or_else(|_| self.try_parse_if_statement()).or_reset(self, current_pos)
            // //     .or_else(|_| self.try_parse_func_call()).or_reset(self, current_pos);

            if let Ok(statement) = parsed {
                result.push(statement);
            } else if let Err(err) = &parsed {
                println!("Error: {:?}", &err);
                last_error = parsed.unwrap_err();
                break;
            }
        }

        if last_error != AstParserError::incompatible_statement(){
            return Err(last_error);
        }

        Ok(StatementAst::Body(BodyAst::new(result)))
    }


    fn try_parse_func_call_as_expression(&mut self) -> Result<ExpressionAst, AstParserError> {
        let func_name = self.expect_token_type(TokenType::Identifier)
            .or_else(|_| Err(AstParserError::incompatible_statement()))?;

        self.expect_token_type(TokenType::BracketOpen)?;

        let params = self.try_parse_func_call_params().unwrap_or(vec![]);

        self.expect_token_type(TokenType::BracketClose)?;

        Ok(ExpressionAst::FuncCall(func_name.value, params))
    }

    fn try_parse_func_call(&mut self) -> Result<StatementAst, AstParserError> {
        let index = self.tokens.get_current_token_index();
        let expr = self.try_parse_func_call_as_expression();

        if let Ok(expr) = expr {
            if let ExpressionAst::FuncCall(name, parameters) = expr {
                self.expect_token_type(TokenType::Semicolon)?;
                return Ok(StatementAst::FunctionCall(FuncCallAst {
                    name,
                    parameters,
                }));
            }
        }

        self.tokens.set_current_token_index(index);
        Err(AstParserError::incompatible_statement())
    }



    pub fn try_parse_func_call_params(&mut self) -> Result<Vec<ExpressionAst>, AstParserError> {
        let mut expressions = vec![];

        loop {
            let current_index = self.tokens.get_current_token_index();
            let expression = self.try_parse_expression()
                .or_reset(self, current_index)
                .or_else(|_| Err(AstParserError::incompatible_statement()))?;


            if expression != ExpressionAst::Undefined {
                expressions.push(expression.clone());
            }

            if let Ok(token) = self.expect_token_type(TokenType::Comma) {
                self.tokens.set_current_token_index(current_index + 1);
            } else {
                break;
            }
        }

        Ok(expressions)
    }


    fn try_parse_func_decl_params(&mut self) -> Result<Vec<ParameterAst>, AstParserError> {
        let mut result : Vec<ParameterAst> = vec![];
        let remember_start = self.tokens.remember();
        loop {
            let mut param_type = self.try_parse_type()?;

            let mut param_name = self.expect_token_type(TokenType::Identifier)?;

            result.push(ParameterAst {
                data_type: param_type,
                name: param_name.value,
            });

            // ended, more params or invalid token
            let mut general_token = self.tokens.next().ok_or_else(|| AstParserError::incompatible_statement())?.clone();
            if  general_token.get_token_type() == TokenType::Comma || general_token.get_token_type() == TokenType::BracketClose {
                // todo: add is_reference check
                // result.push(ParameterAst {
                //     data_type: DataTypeAst {},
                //     name: "".to_string(),
                // });

                if general_token.get_token_type() == TokenType::BracketClose {
                    self.tokens.prev();
                    break;
                }
            } else {
                self.tokens.forget_until(remember_start);
                return Err(AstParserError::token_expected(", or )".to_string(), general_token.value, self.tokens.position()));
            }
        }

        Ok(result)
    }

    fn try_parse_entry_func(&mut self) -> Result<StatementAst, AstParserError> {
        self.expect_token_value("entry")
            .map_err(|_| AstParserError::incompatible_statement())?;

        // bracket open
        self.expect_token_type(TokenType::CurlyBracketOpen)?;

        // body
        let body = self.try_parse_local_body()
            .unwrap_or_else(|_| StatementAst::Body(BodyAst::new(vec![])));

        // bracket open
        self.expect_token_type(TokenType::CurlyBracketClose)?;

        if let Body(body) = body {
            return Ok(StatementAst::FunctionDef(FuncDefAst {
                name: "_entry".to_string(),
                parameters: vec![],
                return_type: DataTypeAst::default_for(DataTypeKind::Void),
                body,
            }));
        }

        Err(AstParserError::unexpected_end_of_file(0))
    }

    fn try_parse_struct_decl(&mut self) -> Result<StatementAst, AstParserError> {
        self.expect_token_value("struct")
            .map_err(|_| AstParserError::incompatible_statement())?;

        let struct_name = self.expect_token_type(TokenType::Identifier)?;

        self.expect_token_type(TokenType::CurlyBracketOpen)?;

        let mut params = vec![];
        loop {
            let data_type = self.try_parse_type()?;
            let name = self.expect_token_type(TokenType::Identifier)?;

            params.push(ParameterAst {
                data_type,
                name: name.value,
            });

            let token = self.expect_token_type(TokenType::Comma)
                .or_else(|_| self.expect_token_type(TokenType::CurlyBracketClose))?;

            if token.get_calculated_token_type() == TokenType::CurlyBracketClose {
                break;
            }
        }

        Ok(StatementAst::StructDecl(StructDeclAst {
            name: struct_name.value,
            params,
        }))
    }

    fn try_parse_function_decl(&mut self) -> Result<StatementAst, AstParserError> {
        let mut func_ret_type = self.try_parse_type()
            .map_err(|_| AstParserError::incompatible_statement())?;

        // function name
        let mut func_name = self.expect_token_type(TokenType::Identifier)?;

        // bracket open
        self.expect_token_type(TokenType::BracketOpen)?;

        // parameters
        let mut parameters = self.try_parse_func_decl_params()
            .unwrap_or(vec![]);

        // bracket close
        self.expect_token_type(TokenType::BracketClose)?;

        // curly bracket open
        self.expect_token_type(TokenType::CurlyBracketOpen)?;

        // body
        let body = self.try_parse_local_body()
            .unwrap_or_else(|_| StatementAst::Body(BodyAst::new(vec![])));

        // curly bracket close
        self.expect_token_type(TokenType::CurlyBracketClose)?;

        if let Body(body) = body {
            Ok(StatementAst::FunctionDef(FuncDefAst {
                name: func_name.value.clone(),
                return_type: func_ret_type,
                parameters,
                body: body,
            }))
        } else {
            Err(AstParserError::unexpected_end_of_file(0))
        }
    }

    fn try_parse_for_loop(&mut self) -> Result<ForLoopAst, AstParserError> {
        self.expect_token_value("for")
            .map_err(|_| AstParserError::incompatible_statement())?;

        self.expect_token_type(TokenType::BracketOpen)?;

        let identifier = self.expect_token_type(TokenType::Identifier)?;

        self.expect_token_type(TokenType::Assign)?;

        let left = self.try_parse_expression()?;

        let loop_type = self.expect_token_type(TokenType::Identifier)?.value.to_lowercase();

        let mut mode = ForLoopMode::To;
        if loop_type == "to" {
            mode = ForLoopMode::To;
        } else if loop_type == "downto" {
            mode = ForLoopMode::DownTo;
        } else {
            return Err(AstParserError::token_expected("to".to_string(), "downto".to_string(), 0));
        }

        let right = self.try_parse_expression()?;

        self.expect_token_type(TokenType::Assign)?;

        let body = self.try_parse_local_body()?;
        if let StatementAst::Body(body) = body {
            self.expect_token_type(TokenType::Assign)?;
            Ok(ForLoopAst {
                var_name: identifier.value,
                start_index: left,
                end_index: right,
                body,
                mode,
            })
        } else {
            Err(AstParserError::incompatible_statement())
        }
    }


    pub fn new(source : &str) -> AstParser {
        let mut tokenizer = Tokenizer::new();

        AstParser {
            tokens: tokenizer.tokenize(source)
        }
    }

    pub fn parse(&mut self) -> Result<ProgramAst, AstParserError> {
        let _last_error : Option<ParseError> = None;

        let mut functions = vec![];
        let mut variables = vec![];
        let mut structs = vec![];
        let mut entry= FuncDefAst {
            name: "".to_string(),
            parameters: vec![],
            return_type: DataTypeAst::default_for(Void),
            body: BodyAst {
                items: vec![]
            },
        };

        loop {
            let current_index = self.tokens.get_current_token_index();

            let res = self.try_parse_var_decl().or_reset(self, current_index)
                .or_else(|_| self.try_parse_entry_func()).or_reset(self, current_index)
                .or_else(|_| self.try_parse_function_decl()).or_reset(self, current_index)
                .or_else(|_| self.try_parse_struct_decl()).or_reset(self, current_index);

            match res {
                Ok(statement) => {
                    println!("parsed ok: {:?}", statement);

                    match statement {
                        StatementAst::VariableDecl(var_decl) => {
                            variables.push(var_decl)
                        }
                        StatementAst::StructDecl(struct_decl) => {
                            structs.push(struct_decl)
                        }
                        StatementAst::FunctionDef(func_decl) => {
                            if func_decl.name == "_entry" {
                                entry = func_decl;
                            } else {
                                functions.push(func_decl)
                            }
                        }
                        _ => {
                            return Err(AstParserError::incompatible_statement())
                        }
                    }
                }
                Err(err) => {
                    if let IncompatibleStatement = err.kind {
                        break;
                    } else if let AstParserErrorKind::EndOfFile = err.kind {
                        break;
                    } else {
                        return Err(err);
                    }
                }
            }

            if !self.tokens.is_currently_in_range() {
                break;
            }
        }

        Ok(ProgramAst {
            entry,
            functions,
            variables,
            structs,
        })
    }
}