val lookahead : 'a list -> 'a option
val match_token : 'a list -> 'a -> 'a list
val lookahead_many : 'a list -> int -> 'a option
val parse_expr : Types.token list -> Types.token list * Types.expr
val parse_add : Types.token list -> Types.token list * Types.expr
val parse_mul : Types.token list -> Types.token list * Types.expr
val parse_pow : Types.token list -> Types.token list * Types.expr
val parse_log : Types.token list -> Types.token list * Types.expr
val parse_trig : Types.token list -> Types.token list * Types.expr
val parse_var : Types.token list -> Types.token list * Types.expr
val parse_const : Types.token list -> Types.token list * Types.expr
val parse : Types.token list -> Types.expr