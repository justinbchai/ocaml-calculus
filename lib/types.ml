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
  | Var of string                   (* e.g. x *)
  | Const of float                  (* e.g. 3.14 *)
  | BinOp of op * expr * expr       (* e.g. x + 3.14 *)
  | UnaryOp of op * expr            (* e.g. sin x *)
