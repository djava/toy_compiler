use clap::Parser;
use clio::*;
use compiler::{parser, pipeline::Pipeline};
use std::io::{Read, Write};

#[derive(Parser, Debug)]
#[clap(name = "compiler")]
struct Args {
    /// Input file path
    input: Input,

    /// Output file path
    #[clap(short, long, default_value = "-")]
    output: Output,

    /// Enable optimization
    #[clap(short = 'O', long)]
    optimize: bool,

    #[clap(long)]
    emit_ast: bool,

    #[clap(long)]
    emit_ir: bool,
}

fn main() {
    let mut args = Args::parse();
    
    if args.emit_ast && args.emit_ir {
        panic!("--emit-ast and --emit-ir are mutually exclusive")
    }

    let mut input_buf = vec![];
    args.input
        .read_to_end(&mut input_buf)
        .expect(format!("Error on reading input file: `{}`", args.input.path()).as_str());

    let input_buf_str = match str::from_utf8(&input_buf) {
        Ok(str) => str,
        Err(e) => panic!("UTF-8 Error in input file: {e:#?}"),
    };
    let ast = match parser::parse(input_buf_str) {
        Ok(ast) => ast,
        Err(e) => panic!("Parse error: {e:#?}"),
    };

    let pipeline = if args.optimize {
        Pipeline::make_full()
    } else {
        Pipeline::make_no_opt()
    };

    
    if args.emit_ast {
        let final_ast = pipeline.run_ast_only(ast);
        write!(args.output, "{final_ast:#?}")
            .expect(format!("Error on writing output file: `{}`", args.output.path()).as_str());
    } else if args.emit_ir {
        let ir_program = pipeline.run_up_to_ir_only(ast);
        write!(args.output, "{ir_program:#?}")
            .expect(format!("Error on writing output file: {}`", args.output.path()).as_str());
    } else {
        let x86_program = pipeline.run(ast);
        write!(args.output, "{x86_program}")
            .expect(format!("Error on writing output file: `{}`", args.output.path()).as_str());
    }
    
}
