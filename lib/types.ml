(* lexer types *)
exception InvalidInputException of string


type token =
  | Tok_RParen
  | Tok_LParen
  | Tok_Add
  | Tok_Sub
  | Tok_Mul
  | Tok_Div
  | Tok_Pow
  | Tok_Sin
  | Tok_Cos
  | Tok_Var of string
  | Tok_Const of float



(* parser & eval types *)
exception DivByZeroError

type op = 
  | Add
  | Sub
  | Mul
  | Div
  | Pow
  | Sin
  | Cos

type var = string


type expr = 
  | Var of string       (* e.g. x *)
  | Const of float      (* e.g. 3.14 *)
  | Add of expr * expr  (* e.g. x + 3.14 *)
  | Sub of expr * expr  (* e.g. x - 3.14 *)
  | Mul of expr * expr  (* e.g. x * 3.14 *)
  | Div of expr * expr  (* e.g. x / 3.14 *)
  | Pow of expr * expr  (* e.g. x ^ 3.14 *)
  | Sin of expr         (* e.g. sin(x) *)
  | Cos of expr         (* e.g. cos(x) *)

  