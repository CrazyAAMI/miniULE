use crate::generating::ir::{BodyIR, ExpressionIR, IRNode, IRNodeKind, IfStatementIR, ProgramIR, VarDefIR};
use crate::parsing::ast::{BodyAst, ExpressionAst, FuncCallAst, FuncDefAst, ProgramAst, StatementAst, VariableDeclAst, VariableDefAst};
use crate::parsing::static_analyzer::{AnalyzerChainResult, AnalyzerInfo};

struct EvaluatedExpression {
    instructions: Vec<IRNode>,
    final_var_name: Option<String>
}

pub struct Transformer {
    ast: ProgramAst,
    info: AnalyzerInfo,
    output: ProgramIR
}

impl Transformer {
    fn new(ast: &ProgramAst, info: &AnalyzerInfo) -> Self {
        Self {
            ast: ast.clone(),
            info: info.clone(),
            output: ProgramIR::new(),
        }
    }

    fn transform_expression(&mut self, expr: &ExpressionAst) -> ExpressionIR {
        match expr {
            ExpressionAst::Undefined => { panic!(""); }
            ExpressionAst::Bracketed(expr) => ExpressionIR::Bracketed(Box::new(self.transform_expression(expr).clone())),
            ExpressionAst::Boolean(bool) => ExpressionIR::ConstInt(*bool as i32),
            ExpressionAst::String(str) => ExpressionIR::ConstStr(str.clone()),
            ExpressionAst::Integer(int) => ExpressionIR::ConstInt(int.clone()),
            ExpressionAst::Identifier(name) => ExpressionIR::Identifier(name.clone()),
            ExpressionAst::FuncCall(_, _) => { panic!(""); }
            ExpressionAst::Binary {
                left,
                operator,
                right
            }
            => ExpressionIR::BinaryExpression(Box::new(self.transform_expression(left)), operator.clone(), Box::new(self.transform_expression(right))),
            ExpressionAst::ArrayDef(array) => {
                let mut exprs = vec![];
                for item in array {
                    exprs.push(self.transform_expression(&item));
                }
                ExpressionIR::ConstArray(exprs)
            }
            _ => {
                panic!("not implemd yet")
            }
        }
    }

    fn transform_var_decl(&mut self, variable_decl: &VariableDeclAst) -> Vec<IRNode> {
        let result = vec![];



        result
    }

    fn handle_global_vars(&mut self) {
        for var_decl in self.ast.variables.clone() {
            let value = self.transform_expression(&var_decl.value).clone();

            self.output.body.items.push(IRNode::var_def(VarDefIR {
                name: var_decl.name.clone(),
                value
            }))
        }
    }


    // fn handle_expression_pre_evaluation(&mut self, expr: &ExpressionAst) {
    //     let mut body = IRNode::body(BodyIR::new(vec![]));
    //
    //     fn handle_expression_pre_evaluation_step(sub_expr: &ExpressionAst, body: &mut BodyIR) {
    //         match sub_expr {
    //             ExpressionAst::Boolean(_) => {}
    //             ExpressionAst::String(_) => {}
    //             ExpressionAst::Integer(_) => {}
    //             ExpressionAst::Identifier(_) => {}
    //             ExpressionAst::FuncCall(name, args) => {
    //                 body.items.push(IRNode::var_def())
    //             }
    //             ExpressionAst::Binary { left, operator, right } =>
    //                 {
    //
    //                 }
    //             _ => panic!("panic during pre evaluation")
    //         }
    //     }
    //
    //     handle_expression_pre_evaluation_step(&expr, &mut body);
    //
    //     body
    // }

    // fn transform_var_def(&mut self, var_def: &VariableDefAst) -> IRNode {
    //     let var_decl = self.info.find_var(&var_def.name);
    //
    //     if let Some(var_decl) = var_decl {
    //         if var_decl.data_type.is_array {
    //
    //         } else {
    //
    //         }
    //     }
    // }

    fn transform_body(&mut self, body: &BodyAst) -> BodyIR {
        let mut items : Vec<IRNode> = vec![];

        for item in &body.items {
            match item {
                StatementAst::Undefined => {}
                StatementAst::VariableDecl(var_decl) => {
                    items.append(&mut self.transform_var_decl(&var_decl));
                }
                StatementAst::VariableDef(var_def) => {
                    items.append(&mut self.handle_transform_var_def(&var_def));
                }
                StatementAst::StructDecl(_) => {}
                StatementAst::StructDef(_) => {}
                StatementAst::IfStatement(if_statement) => {
                    items.push(IRNode::if_statement(IfStatementIR {
                        condition: ExpressionIR::from_ast_expression(&if_statement.condition),
                        then_body: self.transform_body(&if_statement.body),
                        else_body: None,
                    }));
                }
                StatementAst::FunctionDef(_) => {}
                StatementAst::FunctionCall(func_call) => {
                    let func = self.info.functions.iter().find(|i| { *i.ast.name == func_call.name });

                    if let Some(func) = func {
                        println!("FUNC  FOUND {}", func_call.name);
                        let mut index = 0;
                        for param in &func.ast.parameters {
                            let call_param = &func_call.parameters[index];

                            items.push(IRNode::var_def(VarDefIR {
                                name: format!("_A_{}_{}", func_call.name, param.name),
                                value: ExpressionIR::from_ast_expression(&call_param),
                            }));
                            index += 1;
                        }

                        items.push(IRNode::body(self.transform_body(&func.ast.body.clone())));
                    } else {
                        println!("FUNC NOT FOUND {}", func_call.name);
                    }


                }
                StatementAst::Body(_) => {}
                StatementAst::Return(_) => {}
                _ => {}
            }
        }
        BodyIR {
            items,
        }
    }

    fn handle_entry_func(&mut self) {
        let body = self.transform_body(&self.ast.entry.body.clone());

        self.output.body.items.push(IRNode::body(body));
        self.output.body.items.push(IRNode::var_def(VarDefIR{
            name: "OUT1.Data".to_string(),
            value: ExpressionIR::Identifier("rs".to_string()),
        }));
    }

    pub fn transform(program_ast: ProgramAst, info: AnalyzerInfo) -> ProgramIR {
        let mut transformer = Transformer::new(&program_ast, &info);
        transformer.handle_global_vars();
        transformer.handle_entry_func();
        transformer.output
    }

    fn handle_transform_left_assignment(expr: &ExpressionAst, ) -> AnalyzerChainResult {
        let result = vec![];

        vec![]
    }

    fn evaluate_right_expression(&mut self, expr: &ExpressionAst) -> EvaluatedExpression {
        let mut result = EvaluatedExpression {
            instructions: vec![],
            final_var_name: None,
        };

        let mut index = 0;
        let mut final_expr = expr;
        if let ExpressionAst::ExpressionChain(chain) = &expr {
            for item in chain {
                if index == 0 {

                } else {

                }
                index += 1;
            }
        } else {
            result.instructions.push(IRNode {
                kind: IRNodeKind::Expression(ExpressionIR::from_ast_expression(expr))
            });
        }

        result
    }

    fn handle_transform_var_def(&mut self, var_def: &VariableDefAst) -> Vec<IRNode> {
        let mut items = vec![];

        items.append(&mut self.transform_right_assignment(&var_def.value));
        items.append(&mut self.transform_right_assignment(&var_def.value));


        let mut final_left_expr = var_def.value.clone();




        items.push(IRNode::var_def(VarDefIR {
            name: "TODOVARNAME".to_string(),
            value: ExpressionIR::from_ast_expression(&final_left_expr),
        }));

        items
    }
}

