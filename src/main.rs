use crate::generating::generator::CodeGenerator;
use crate::generating::transformer::Transformer;
use crate::parsing::parser::{AstParser, AstParserError};
use crate::parsing::static_analyzer::Analyzer;
use crate::parsing::tokenizer::Tokenizer;

mod parsing;
mod transforming;
mod generating;
mod analyzing;
mod optimizing;

fn perform(input: &str) {
    let mut parser = AstParser::new(&input);
    match parser.parse() {
        Ok(ast) => {
            println!("{:#?}", ast);
            let mut analyzer = Analyzer::analyze(ast.clone());
            if let Err(e) = analyzer.clone() {
                println!("Analyzer Error {:#?}", e);
            } else if let Ok(info) = analyzer.clone() {
                println!("[Analyzer] Completed!\n{:#?}", info);
                println!("[Transformer] Starting...");

                let program_ir = Transformer::transform(ast.clone(), info.clone());
                println!("[Transformer]\n{:?}", program_ir);

                println!("[Generator] Initializing...");
                let mut generator = CodeGenerator::new(ast, info, program_ir);
                let test = generator.generate();
                println!("[Generated]\n{}", test);

            }
        }
        Err(err) => {
            println!("[PARSER] Compilation failed parsing:\r[PARSER] - {}\r[PARSER] - Pos: {}", err.to_message(), err.token_position)
        }
    }
}

fn main() {
    println!("hyperULE Compiler v0.1");
    println!("----------------------");
    println!("{}", concat!(
    "Compile hyperULE to Datalogic User Label Edit scripts by running this compiler",
    "with a .hule file as first parameter or with the start up parameters ",
    "-i <input file> / -i <input_file> -o <output_file>."));
    println!("");


    let input = "entry{string test = \"\";}";
    let input = "int global_var = \"some global var\";entry{greet(\"world\");}void greet(string name) {string name = \"fk\";string result = \"hello \" + name + \"!!!\";result=\"test 222\";}";
    perform(input);

    loop {
        let mut input = String::new();
        match std::io::stdin().read_line(&mut input) {
            Ok(_goes_into_input_above) => {},
            Err(_no_updates_is_fine) => {},
        }


        input = input.trim().to_string();
        perform(&input);
    }
}