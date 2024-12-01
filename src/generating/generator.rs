use crate::generating::ir::{*};
use crate::parsing::ast::ProgramAst;
use crate::parsing::static_analyzer::AnalyzerInfo;

pub enum CodeGeneratorError {
    Unknown
}

pub enum CodeOutputKind {
    Line(String),
    Group(Vec<CodeOutputKind>),
    Block(String, Vec<CodeOutputKind>)
}


#[derive(Clone, Debug)]
struct CodeGeneratorSettings {
    indent_spaces: usize,
    indent_level: usize
}

impl CodeGeneratorSettings {
    fn new() -> CodeGeneratorSettings {
        CodeGeneratorSettings {
            indent_spaces: 4,
            indent_level: 0
        }
    }
}

#[derive(Clone, Debug)]
struct CodeGeneratorBootstrap {
    call_stack: VarDefIR,
    fc: VarDefIR,
}

impl CodeGeneratorBootstrap {
    fn default() -> CodeGeneratorBootstrap {
        CodeGeneratorBootstrap {
            call_stack: VarDefIR {
                name: "call_stack".to_string(),
                value: ExpressionIR::ConstStr("\"\\x00\"".to_string())
            },
            fc: VarDefIR {
                name: "fc".to_string(),
                value: ExpressionIR::ConstInt(0)
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct CodeGenerator {
    settings: CodeGeneratorSettings,
    ast: ProgramAst,
    analyzer: AnalyzerInfo,
    program_ir: ProgramIR,
    buffer: String,
    bootstrap: CodeGeneratorBootstrap
}

impl CodeGenerator {
    pub fn write(&mut self, line: &str) -> &mut Self {
        let indent = " ".repeat(self.settings.indent_level * self.settings.indent_spaces);
        self.buffer.push_str(&indent);
        self.buffer.push_str(line);
        self
    }

    pub fn write_line(&mut self, line: &str) -> &mut Self {
        let indent = " ".repeat(self.settings.indent_level * self.settings.indent_spaces);
        self.buffer.push_str(&indent);
        self.buffer.push_str(line);
        self.buffer.push('\n');
        self
    }
    pub fn write_empty_line(&mut self) -> &mut Self {
        self.buffer.push('\n');
        self
    }

    pub fn inc_indent(&mut self) -> &mut Self {
        self.settings.indent_level = self.settings.indent_level + 1;
        self
    }

    pub fn dec_indent(&mut self) -> &mut Self {
        self.settings.indent_level = self.settings.indent_level - 1;
        self
    }

    pub fn convert_expression(expr: ExpressionIR) -> String {
        match expr {
            ExpressionIR::Identifier(identifier) => identifier,
            ExpressionIR::ConstStr(str) => str,
            ExpressionIR::ConstInt(int) => format!("{}", int),
            ExpressionIR::Bracketed(bracketed) => format!("({})", Self::convert_expression(*bracketed)),
            ExpressionIR::BinaryExpression(left, op, right) => format!("{}{}{}", Self::convert_expression(*left), op.to_str(), Self::convert_expression(*right)),
            ExpressionIR::FuncCallExpression(_, _) => {
                "function call not implemented yet".to_string()
            }
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
                parts.join("+").replace("\"+\"", "")
            }
        }
    }

    pub fn convert_expressions(expressions: &Vec<ExpressionIR>) -> String {
        let mut result = "".to_string();

        for expr in expressions {
            result.push_str(&Self::convert_expression(expr.clone()))
        }

        result
    }


    fn handle_var_def(&mut self, var_ir: &VarDefIR) {
        self.write_line(&format!("{}={}", &var_ir.name, Self::convert_expression(var_ir.value.clone())));
    }

    pub fn write_if_statement(&mut self, if_statement_ir: &IfStatementIR) {
        self.write_line(&format!("if ({}) {{", if_statement_ir.condition.to_code_str())).inc_indent();
        self.write_body(&if_statement_ir.then_body);
        self.dec_indent().write_line("}");
    }

    pub fn write_body(&mut self, routine: &BodyIR) {
        for item in routine.items.clone() {
            match item.kind {
                IRNodeKind::Block(_) => {}
                IRNodeKind::VarDef(var_def) => {
                    self.handle_var_def(&var_def);
                }
                IRNodeKind::WhileLoop(_) => {}
                IRNodeKind::IfStatement(if_statement) => {
                    self.write_if_statement(&if_statement);
                }
                IRNodeKind::Expression(_) => {}
                IRNodeKind::Body(body) => {
                    self.write_body(&body);
                }
            }
        }
    }


    pub fn handle_program(&mut self) {
        self.write_line("{").inc_indent();

        self.write_body(&self.program_ir.body.clone());

        self.dec_indent().write_line("}");
    }

    pub fn handle_comment(&mut self, str: &str) {
        self.write_line(&format!("# {}", str));
    }

    pub fn comment_on_last_line(&mut self, str: &str) {
        self.buffer.insert_str(self.buffer.len() - 1, &format!(" # {}", str));
    }

    pub fn handle_bootstrap(&mut self) {
        self.handle_comment("Runtime variables");
        self.handle_comment("------------------");
        self.handle_var_def(&self.bootstrap.call_stack.clone());
        self.comment_on_last_line("Call Stack");
        self.handle_var_def(&self.bootstrap.fc.clone());
        self.comment_on_last_line("Function Counter");
        self.write_empty_line();
    }

    pub fn generate(&mut self) -> String {
        self.handle_program();
        self.buffer.clone()
    }

    pub fn new(ast: ProgramAst, analyzer: AnalyzerInfo, program_ir: ProgramIR) -> CodeGenerator {
        let mut generator = CodeGenerator {
            settings: CodeGeneratorSettings::new(),
            ast,
            analyzer,
            program_ir,
            buffer: "".to_string(),
            bootstrap: CodeGeneratorBootstrap::default(),
        };

        generator
    }
}

