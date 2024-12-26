  open Types

  let lookahead (toks) = 
    match toks with [] -> None
    | h :: t -> Some h

  let match_token toks tok =
    match toks with
    | [] -> raise (InvalidInputException ("Invalid input"))
    | h :: t when h = tok -> t
    | h :: _ -> raise (InvalidInputException ("Invalid input"))

  let rec lookahead_many toks n =
    match (toks, n) with
    | h :: _, 0 -> Some h
    | _ :: t, n when n > 0 -> lookahead_many t (n - 1)
    | _ -> None

  (* 
  CFG FOR MATH EXPRESSIONS
  Expr -> AddExpr
  AddExpr -> Expr AddOperator Expr | MulExpr
  AddOperator -> + | -
  MulExpr -> Expr MultOperator Expr | DivExpr
  MultOperator -> * | /
  PowExpr -> Expr PowerOperator Expr | TrigExpr
  PowerOperator -> ^
  TrigExpr -> SinExpr | CosExpr | VarExpr
  SinExpr -> sin(Expr)
  CosExpr -> cos(Expr)
  VarExpr -> [a-z] | ConstExpr
  ConstExpr -> const | (Expr)
  *)

  (* 
  e.g 1 * (2 + 3)
  1. parse_expr -> parse_mul -> parse_pow -> parse_trig -> parse_var -> parse_const -> 
   *)
  let rec parse_expr toks = 
    parse_add toks

  and parse_add toks =
    let t1, e1 = parse_mul toks in
    match lookahead t1 with
    | Some Tok_Add -> 
      let t2 = match_token t1 Tok_Add in
      let t3, e2 = parse_add t2 in
      t3, BinOp (Add, e1, e2)
    | Some Tok_Sub ->
      let t2 = match_token t1 Tok_Sub in
      let t3, e2 = parse_add t2 in
      t3, BinOp (Sub, e1, e2)
    | _ -> t1, e1

  and parse_mul toks =
    let t1, e1 = parse_log toks in
    match lookahead t1 with
    | Some Tok_Mul -> 
      let t2 = match_token t1 Tok_Mul in
      let t3, e2 = parse_mul t2 in
      t3, BinOp (Mul, e1, e2)
    | Some Tok_Div ->
      let t2 = match_token t1 Tok_Div in
      let t3, e2 = parse_mul t2 in
      t3, BinOp (Div, e1, e2)
    | _ -> t1, e1

  and parse_log toks =
    match lookahead toks with
    | Some Tok_Log ->
      let t1 = match_token toks Tok_Log in
      let t2, e1 = parse_log t1 in
      let t3, e2 = parse_log t2 in
      t3, BinOp (Log, e1, e2)
    | _ -> parse_pow toks

  and parse_pow toks =
    let t1, e1 = parse_trig toks in
    match lookahead t1 with
    | Some Tok_Pow -> 
      let t2 = match_token t1 Tok_Pow in
      let t3, e2 = parse_pow t2 in
      t3, BinOp (Pow, e1, e2)
    | _ -> t1, e1
    
  and parse_trig toks =
    match lookahead toks with
    | Some Tok_Sin -> 
      let t1 = match_token toks Tok_Sin in
      let t2, e = parse_trig t1 in
      t2, UnaryOp (Sin, e)
    | Some Tok_Cos ->
      let t1 = match_token toks Tok_Cos in
      let t2, e = parse_trig t1 in
      t2, UnaryOp (Cos, e)
    | _ -> parse_var toks

  and parse_var toks =
    match lookahead toks with
    | Some (Tok_Var v) -> 
      let t1 = match_token toks (Tok_Var v) in
      t1, Var v
    | _ -> parse_const toks

  and parse_const toks =
    match lookahead toks with
    | Some (Tok_Const c) -> 
      let t1 = match_token toks (Tok_Const c) in
      t1, Const c
    | Some Tok_LParen ->
      let t1 = match_token toks Tok_LParen in
      let t2, e = parse_expr t1 in
      let t3 = match_token t2 Tok_RParen in
      t3, e
    | _ -> raise (InvalidInputException ("Invalid input"))