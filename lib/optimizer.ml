  open Types
  

  let is_int (x: float) : bool = 
    let y = int_of_float x in
    x = float_of_int y

  let rec optimize e =
    match e with
    | Const (Float x) -> if is_int x then Const (Int (int_of_float x)) else e
    | Const (_) | Var (_)-> e
    | BinOp (op, e1, e2) ->
      (match op with
      | Add -> 
        (match e1, e2 with
        | Const (Int 0), _ -> optimize e2
        | _, Const (Int 0) -> optimize e1
        | Const (Int x), Const (Int y) -> Const (Int (x + y))
        | Const (Float x), Const (Float y) -> Const (Float (x +. y))
        | Const (Float x), Const (Int y) -> Const (Float (x +. float_of_int y))
        | Const (Int x), Const (Float y) -> Const (Float (float_of_int x +. y))
        | _, _ -> BinOp (Add, optimize e1, optimize e2))
      | Sub -> 
        (match e1, e2 with
        | _, Const (Int 0) -> optimize e1
        | Const (Int x), Const (Int y) -> Const (Int (x - y))
        | Const (Float x), Const (Float y) -> Const (Float (x -. y))
        | Const (Float x), Const (Int y) -> Const (Float (x -. float_of_int y))
        | Const (Int x), Const (Float y) -> Const (Float (float_of_int x -. y))
        | _, _ -> BinOp (Sub, optimize e1, optimize e2))
      | Mul -> 
        (match e1, e2 with
        | Const (Int 0), _ -> Const (Int 0)
        | _, Const (Int 0) -> Const (Int 0)
        | Const (Int 1), _ -> optimize e2
        | _, Const (Int 1) -> optimize e1
        | Const (Int x), Const (Int y) -> Const (Int (x * y))
        | Const (Int x), Const (Float y) -> Const (Float (float_of_int x *. y))
        | Const (Float x), Const (Int y) -> Const (Float (x *. float_of_int y))
        | Const (Float x), Const (Float y) -> Const (Float (x *. y))
        | _, _ -> BinOp (Mul, optimize e1, optimize e2))
      | Div -> 
        (match e1, e2 with
        | _, Const (Int 1) -> optimize e1
        | Const (Int 0), _ -> Const (Int 0)
        | _, Const (Int 0) -> raise DivByZeroError
        | Const (Int x), Const (Int y) -> Const (Int(x / y))
        | Const (Float x), Const (Float y) -> Const (Float (x /. y))
        | Const (Float x), Const (Int y) -> Const (Float (x /. float_of_int y))
        | Const (Int x), Const (Float y) -> Const (Float (float_of_int x /. y))
        | _, _ -> BinOp (Div, optimize e1, optimize e2))
      | Log -> 
        (match e1, e2 with
        | _, Const (Int 1) -> Const (Int 0)
        | Const (x), Const (y) -> if x = y then Const (Int 1) else e
        | _, _ -> BinOp (Log, optimize e1, optimize e2))
      | Pow -> 
        (match e1, e2 with
        | _, Const (Int 0) -> Const (Int 1)
        | _, Const (Int 1) -> optimize e1
        | Const (Int 0), _ -> Const (Int 0)
        | Const (Int x), Const (Int y) -> Const (Float (float_of_int x ** float_of_int y))
        | _, _ -> BinOp (Pow, optimize e1, optimize e2))
      )
    | UnaryOp (op, e1) ->
      (match op with
      | Sin -> UnaryOp (Sin, optimize e1)
      | Cos -> UnaryOp (Cos, optimize e1)
      )