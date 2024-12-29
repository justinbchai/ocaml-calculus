open Types
open Optimizer
open Interface
open Parser
open Lexer

(* differentiate e with respect to v *)
let rec eval_diff e v =
  let e = optimize e in
  match e with
  | Const _ -> Const (Int 0)
  | Var x -> if x = v then Const (Int 1) else Const (Int 0)
  | BinOp (op, e1, e2) ->
    (match op with
    (* diff(u(x) + v(x)) = diff(u(x)) + diff(v(x)) *)
    | Add -> 
      print_endline ("We can differentiate summands separately and pull out constant factors: " ^ string_of_expr e);
      BinOp (Add, eval_diff e1 v, eval_diff e2 v)
    (* diff(u(x) - v(x)) = diff(u(x)) - diff(v(x)) *)
    | Sub -> 
      print_endline ("We can differentiate summands separately and pull out constant factors: " ^ string_of_expr e);
      BinOp (Sub, eval_diff e1 v, eval_diff e2 v)
    (* diff(u(x) * v(x)) = u(x) * diff(v(x)) + v(x) * diff(u(x)) *)
    | Mul -> 
      print_endline ("Applying the product rule on: " ^ string_of_expr e);
      BinOp (Add, BinOp (Mul, e1, eval_diff e2 v), BinOp (Mul, eval_diff e1 v, e2))
    (* diff(u(x) / v(x)) = (diff(u(x)) * v(x) - u(x) * diff(v(x))) / v(x)^2 *)
    | Div -> 
      print_endline ("Applying the quotient rule on: " ^ string_of_expr e);
      BinOp (Div, BinOp (Sub, BinOp (Mul, eval_diff e1 v, e2), BinOp (Mul, e1, eval_diff e2 v)), BinOp (Pow, e2, Const (Int 2)))
    | Log ->
      print_endline ("Applying the differentiation rule: [ln(u(x))]' = u'(x)/u(x) on: " ^ string_of_expr e);
      (match e1 with
      | Const (String "e") -> BinOp (Div, eval_diff e2 v, e2)
      (* diff(log(a, b)) = diff(ln(b)/ln(a)) *)
      | _ -> eval_diff (BinOp(Div, BinOp(Log, Const(String "e"), e2), BinOp(Log, Const(String "e"), e1))) v)
    | Pow -> 
      print_endline ("Applying the generalized power rule [u(x)^v(x)]' = u(x)^v(x) * [ln(u(x))*v(x)]' on: " ^ string_of_expr e);
      (match e2 with
      | Const (Int n) -> BinOp (Mul, Const (Int n), BinOp (Pow, e1, Const (Int (n - 1))))
      | Const (Float f) -> BinOp (Mul, Const (Float f), BinOp (Pow, e1, Const (Float (f -. 1.0))))
      (* generalized power rule: diff(u(x)^(v(x))) = u(x)^(v(x)) * diff(ln(u(x))*v(x)) *)
      | _ -> BinOp (Mul, BinOp(Pow, e1, e2), eval_diff (BinOp(Mul, BinOp (Log, Const(String("e")), e1), e2)) v))
    )
  | UnaryOp (op, e1) ->
    print_endline ("Applying trig derivative rules on: " ^ string_of_expr e);
      (match op with
      | Sin -> BinOp (Mul, eval_diff e1 v, UnaryOp(Cos, e1))
      | Cos -> BinOp (Mul, eval_diff e1 v, BinOp(Sub, Const(Int 0), UnaryOp(Sin,e1)))
      )

let get_diff str v =
  let e = parse (tokenize str) in 
  print_endline (string_of_expr (optimize(eval_diff e v)))