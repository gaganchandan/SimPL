open SimPL

let parse source =
  let lexbuf = Lexing.from_string source in
  let ast = SimPL.Parser.prog SimPL.Lexer.read lexbuf in
  ast
