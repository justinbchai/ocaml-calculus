open Types

let lookahead (toks) = 
  match toks with [] -> None
  | h :: t -> Some h

(* 
CFG FOR MATH EXPRESSIONS
Expr -> AddExpr | SubExpr | MulExpr | DivExpr | PowExpr | SinExpr | CosExpr | VarExpr | ConstExpr
*)
let rec parse_expr toks = 
  match lookahead toks with
  | Some Tok_Add -> parse_add toks
  | Some Tok_Sub -> parse_sub toks
  | Some Tok_Mul -> parse_mul toks
  | Some Tok_Div -> parse_div toks
  | Some Tok_Pow -> parse_pow toks
  | Some Tok_Sin -> parse_sin toks
  | Some Tok_Cos -> parse_cos toks
  | Some (Tok_Var _) -> parse_var toks
  | Some (Tok_Const _) -> parse_const toks
  | _ -> raise (InvalidInputException "Invalid input")