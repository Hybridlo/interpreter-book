use std::io::{BufRead as _, BufReader, Read, Write};

use crate::{evaluator::eval, object::environment::Environment, parser::Parser};

use itertools::Itertools as _;

const PROMPT: &str = ">> ";

pub fn start(input: impl Read, mut output: impl Write) {
    let mut scanner = BufReader::new(input);
    let mut line_buf = String::new();
    let mut env = Environment::new();

    loop {
        line_buf.clear();

        write!(output, "{}", PROMPT).expect("Writing to output in REPL failed");
        output.flush().expect("Flushing output in REPL failed");
        let scanned = scanner
            .read_line(&mut line_buf)
            .expect("Reading a line failed");
        if scanned == 0 {
            return;
        }

        let parser = Parser::new(&line_buf);
        let (program, errors) = parser.parse_program();
        if !errors.is_empty() {
            write!(
                output,
                "{}",
                errors
                    .iter()
                    .map(|err| format!("{:?}", err))
                    .intersperse("\n\t".to_string())
                    .collect::<String>()
            )
            .expect("Writing to output in REPL failed");
            output.flush().expect("Flushing output in REPL failed");
        }

        let evaluated = eval(program.into(), &mut env);

        writeln!(output, "{}", evaluated).expect("Writing to output in REPL failed");
        output.flush().expect("Flushing output in REPL failed");
    }
}
