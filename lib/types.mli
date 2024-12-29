exception InvalidInputException of string
type const = Int of int | Float of float | String of string
type token =
    Tok_RParen
  | Tok_LParen
  | Tok_Add
  | Tok_Sub
  | Tok_Mul
  | Tok_Div
  | Tok_Pow
  | Tok_Sin
  | Tok_Cos
  | Tok_Log
  | Tok_Var of string
  | Tok_Const of const
exception DivByZeroError
type op = Add | Sub | Mul | Div | Pow | Sin | Cos | Log
type expr =
    Var of string
  | Const of const
  | BinOp of op * expr * expr
  | UnaryOp of op * expr
