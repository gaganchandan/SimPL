open SimPL
open SimPL.Ast

let parse source =
  let lexbuf = Lexing.from_string source in
  let ast = SimPL.Parser.prog SimPL.Lexer.read lexbuf in
  ast

let is_value = function
  | Int _ | Bool _ -> true
  | Var _ | Binop _ | If _ | Let _ -> false

let subst _ _ _ = failwith "TODO"

let rec step = function
  | Int _ | Bool _ -> failwith "No step"
  | Var _ -> failwith "Unbound variable"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop (bop, step e1, e2)
  | If (Bool true, e1, _) -> e1
  | If (Bool false, _, e2) -> e2
  | If (Int _, _, _) -> failwith "Condition must be a boolean"
  | If (e1, e2, e3) -> If (step e1, e2, e3)
  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)

and step_bop bop e1 e2 =
  match (bop, e1, e2) with
  | Add, Int x, Int y -> Int (x + y)
  | Mul, Int x, Int y -> Int (x * y)
  | Leq, Int x, Int y -> Bool (x <= y)
  | _ -> failwith "operand and operator type mismatch"

let rec eval_small e = if is_value e then e else eval_small (step e)

let rec eval_big e =
  match e with
  | Int _ | Bool _ -> e
  | Var _ -> failwith "Unbound variable"
  | Binop (bop, e1, e2) -> eval_bop bop e1 e2
  | Let (x, e1, e2) -> subst e2 (eval_big e1) x |> eval_big
  | If (e1, e2, e3) -> eval_if e1 e2 e3
