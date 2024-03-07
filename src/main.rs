mod ast;
mod lexer;
mod repl;
mod token;
mod parser;

fn main() {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to type in commands");
    repl::start(std::io::stdin(), std::io::stdout())
}
