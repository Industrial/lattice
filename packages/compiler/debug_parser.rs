use lattice_compiler::lexer::Lexer;
use lattice_compiler::parser::Parser;

fn main() {
  let source = "let x = 42 in x + 1";
  println!("Testing input: {}", source);

  // Test lexer
  let lexer = Lexer::from_str(source);
  let tokens: Result<Vec<_>, _> = lexer.collect();
  match tokens {
    Ok(tokens) => {
      println!("Tokens: {:?}", tokens);

      // Test parser
      let mut parser = Parser::new();
      let result = parser.parse(&tokens);
      match result {
        Ok(ast) => {
          println!("Success! AST: {:?}", ast);
        }
        Err(err) => {
          println!("Parse error: {:?}", err);
        }
      }
    }
    Err(err) => {
      println!("Lexer error: {:?}", err);
    }
  }
}
