  open Types

  let rec string_of_expr e =
    match e with
    | Const (Int n) -> string_of_int n
    | Const (Float f) -> string_of_float f
    | Const (String s) -> s
    | Var x -> x
    | BinOp (op, e1, e2) ->
      (match op with
      | Add -> "(" ^ string_of_expr e1 ^ " + " ^ string_of_expr e2 ^ ")"
      | Sub -> if e1 <> Const(Int 0) then "(" ^ string_of_expr e1 ^ " - " ^ string_of_expr e2 ^ ")" else "-" ^ string_of_expr e2
      | Mul -> "(" ^ string_of_expr e1 ^ " * " ^ string_of_expr e2 ^ ")"
      | Div -> "(" ^ string_of_expr e1 ^ " / " ^ string_of_expr e2 ^ ")"
      | Log -> "log(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
      | Pow -> "(" ^ string_of_expr e1 ^ " ^ " ^ string_of_expr e2 ^ ")"
      )
    | UnaryOp (op, e1) ->
      (match op with
      | Sin -> "sin(" ^ string_of_expr e1 ^ ")"
      | Cos -> "cos(" ^ string_of_expr e1 ^ ")"
      )
