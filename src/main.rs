#![allow(unused_imports, dead_code, unused_assignments)]
use std::{env, process::exit, fs, io::Write};
mod error;
mod lexer;
mod structure;
mod map;
mod eval;
use error::*;

fn execute(path: &String, text: String, program: &mut eval::Program) -> Result<Option<eval::Value>, Error> {
    let tokens = lexer::lex(path, text)?;
    let instrs = structure::parse(path, tokens)?;
    // println!("{}", instrs.iter().map(|x| x.to_string()).collect::<Vec<String>>().join("\n"));
    program.instrs(instrs)
}

fn run() -> Result<(), Error> {
    let args: Vec<String> = env::args().collect();
    let mut args = args.iter();
    args.next();
    match args.next() {
        Some(path) => match fs::read_to_string(path) {
            Ok(text) => {
                let mut program = eval::std_program(Some(path));
                if let Some(value) = execute(path, text, &mut program)? {
                    println!("{value}");
                }
            }
            Err(err) => return Err(Error::msg(format!("error while opening file {path:?}: {err}")))
        }
        None => {
            let mut program = eval::std_program(None);
            loop {
                let mut text = String::new();
                print!("> ");
                std::io::stdout().flush().map_err(|err| Error::msg(err.to_string()))?;
                std::io::stdin().read_line(&mut text).map_err(|err| Error::msg(err.to_string()))?;
                match execute(&"<stdin>".into(), text, &mut program) {
                    Ok(Some(value)) => {
                        println!("{value}");
                        program.define("ANS".into(), value, true);
                    }
                    Ok(None) => {},
                    Err(err) => eprintln!("ERROR: {err}")
                }
            }
        }
    }
    Ok(())
}

fn main() {
    if let Some(err) = run().err() {
        eprintln!("ERROR: {err}");
        exit(1);
    }
}