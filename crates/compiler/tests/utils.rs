pub use std::collections::VecDeque;
use std::io::Write;
use tempfile::NamedTempFile;

use compiler::syntax_trees::x86::X86Program;
use test_support::{
    compiler::{pipeline::Pipeline, syntax_trees::parser},
    x86_interpreter::interpret_x86,
};

#[derive(Debug, Clone)]
pub struct TestCase<'a> {
    pub input: &'a str,
    pub inputs: VecDeque<i64>,
    pub expected_outputs: VecDeque<i64>,
}

pub fn execute_test_case(mut tc: TestCase) {
    println!("{}\n-------", tc.input);

    let ast = match parser::parse(tc.input) {
        Ok(ast) => ast,
        Err(e) => panic!("Parse error: {e:#?}"),
    };

    let no_opt_pipeline = Pipeline::make_no_opt();
    let no_opt_x86 = no_opt_pipeline.run(ast.clone());
    println!("-- No Opt: -- \n{}\n", no_opt_x86);

    let mut no_opt_tc = tc.clone();
    let mut no_opt_outputs = VecDeque::<i64>::new();
    interpret_x86(&no_opt_x86, &mut no_opt_tc.inputs, &mut no_opt_outputs);
    assert_eq!(no_opt_outputs, no_opt_tc.expected_outputs);
    let no_opt_assembler_res = run_assembler(no_opt_x86);
    if let Err(e) = no_opt_assembler_res {
        panic!("Failed assembler: {e}")
    }

    let opt_pipeline = Pipeline::make_opt();
    let opt_x86 = opt_pipeline.run(ast);
    println!("-- With Opt: -- \n{}\n", opt_x86);

    let mut opt_outputs = VecDeque::<i64>::new();
    interpret_x86(&opt_x86, &mut tc.inputs, &mut opt_outputs);
    assert_eq!(opt_outputs, tc.expected_outputs, "Failed in optimized only");
    let opt_assembler_res = run_assembler(opt_x86);
    if let Err(e) = opt_assembler_res {
        panic!("Failed assembler in optimized only: {e}")
    }
}

fn run_assembler(p: X86Program) -> Result<(), String> {
    let asm = format!("{p}");

    let out_file = NamedTempFile::new().map_err(|e| format!("failed to create temp file: {e}"))?;
    
    let (program, args) = if cfg!(target_os = "linux") {
        ("gcc", vec![
            "-x",
            "assembler",
            "-c",
            "-o",
            out_file.path().to_str().unwrap(),
            "-",
        ])
    } else if cfg!(target_os = "windows") {
        // Run in WSL if we're on windows
        ("wsl", vec![
            "gcc",
            "-x",
            "assembler",
            "-c",
            "-o",
            out_file.path().to_str().unwrap(),
            "-",
        ])
    } else {
        panic!("Running on unexpected OS")
    };

    let mut asm_command = std::process::Command::new(program)
        .args(args)
        .stdin(std::process::Stdio::piped())
        .stdout(std::process::Stdio::piped())
        .stderr(std::process::Stdio::piped())
        .spawn()
        .map_err(|e| format!("failed to spawn gcc: {e}"))?;

    asm_command
        .stdin
        .take()
        .unwrap()
        .write_all(asm.as_bytes())
        .map_err(|e| format!("Failed to write to stdin: {e}"))?;

    let output = asm_command
        .wait_with_output()
        .map_err(|e| format!("Failed to wait for gcc: {e}"))?;

    if output.status.success() {
        Ok(())
    } else {
        Err(String::from_utf8_lossy(&output.stderr).into_owned())
    }
}
