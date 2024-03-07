use std::io::{BufRead as _, BufReader, Read, Write};

use crate::{lexer::Lexer, token::Token};

const PROMPT: &str = ">> ";

pub fn start(input: impl Read, mut output: impl Write) {
    let mut scanner = BufReader::new(input);
    let mut line_buf = String::new();

    loop {
        write!(output, "{}", PROMPT).expect("Writing to output in REPL failed");
        output.flush().expect("Flushing output in REPL failed");
        let scanned = scanner
            .read_line(&mut line_buf)
            .expect("Reading a line failed");
        if scanned == 0 {
            return;
        }

        let lexer = Lexer::new(&line_buf);

        for token in lexer {
            if token != Token::Eof {
                writeln!(output, "{:?}", token).expect("Writing to output in REPL failed");
                output.flush().expect("Flushing output in REPL failed");
            } else {
                break;
            }
        }
    }
}
