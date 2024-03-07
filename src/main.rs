mod lexer;
mod token;
mod repl;

fn main() {
    println!("Hello! This is the Monkey programming language!");
    println!("Feel free to type in commands");
    repl::start(std::io::stdin(), std::io::stdout())
}
