open Types

let rec tokenize input = 
  let input = String.trim input in
  let len = String.length input in

  let re_float = Re.compile(Re.Perl.re"^([0-9]+\\.[0-9]+)|(\\(-[0-9]\\.[0-9]\\))") in
  let re_int = Re.compile(Re.Perl.re"(^[0-9]+)|(^\\(-[0-9]+\\))") in
  let re_rparen = Re.compile(Re.Perl.re"^\\)") in
  let re_lparen = Re.compile(Re.Perl.re"^\\(") in
  let re_add = Re.compile(Re.Perl.re"^\\+") in
  let re_sub = Re.compile(Re.Perl.re"^-") in
  let re_mul = Re.compile(Re.Perl.re"^\\*") in
  let re_div = Re.compile(Re.Perl.re"^/") in
  let re_pow = Re.compile(Re.Perl.re"^\\^") in
  let re_sin = Re.compile(Re.Perl.re"^sin") in
  let re_cos = Re.compile(Re.Perl.re"^cos") in
  let re_var = Re.compile(Re.Perl.re"^[a-z]") in

  if input = "" then []
  else if Re.execp re_float input then
    let numgroup = Re.exec re_float input in
    let num = Re.Group.get numgroup 0 in
    let numlen = String.length num in
    if String.get num 0 = '(' then
      let res = float_of_string (String.sub num 1 (numlen - 2)) in
      Tok_Const(Float res) :: tokenize (String.sub input numlen (len - numlen))
    else 
      let res = float_of_string num in
      Tok_Const(Float res) :: tokenize (String.sub input numlen (len - numlen))
  else if Re.execp re_int input then
    let numgroup = Re.exec re_int input in
    let num = Re.Group.get numgroup 0 in
    let numlen = String.length num in
    if String.get num 0 = '(' then
      let res = int_of_string (String.sub num 1 (numlen - 2)) in
      Tok_Const(Int res) :: tokenize (String.sub input numlen (len - numlen))
    else 
      let res = int_of_string num in
      Tok_Const(Int res) :: tokenize (String.sub input numlen (len - numlen))
    else if Re.execp re_rparen input then
      Tok_RParen :: tokenize (String.sub input 1 (len - 1))
    else if Re.execp re_lparen input then
      Tok_LParen :: tokenize (String.sub input 1 (len - 1))
    else if Re.execp re_add input then
      Tok_Add :: tokenize (String.sub input 1 (len - 1))
    else if Re.execp re_sub input then
      Tok_Sub :: tokenize (String.sub input 1 (len - 1))
    else if Re.execp re_mul input then
      Tok_Mul :: tokenize (String.sub input 1 (len - 1))
    else if Re.execp re_div input then
      Tok_Div :: tokenize (String.sub input 1 (len - 1))
    else if Re.execp re_pow input then
      Tok_Pow :: tokenize (String.sub input 1 (len - 1))
    else if Re.execp re_sin input then
      Tok_Sin :: tokenize (String.sub input 3 (len - 3))
    else if Re.execp re_cos input then
      Tok_Cos :: tokenize (String.sub input 3 (len - 3))
    else if Re.execp re_var input then
      let vargroup = Re.exec re_var input in
      let var = Re.Group.get vargroup 0 in
      Tok_Var(var) :: tokenize (String.sub input (String.length var) (len - (String.length var)))
    else
      raise (InvalidInputException "Invalid input")