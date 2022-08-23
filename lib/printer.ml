open Type

let token_to_str (tok : token) =
  match tok with
  | TOK_PRINT -> "Print"
  | TOK_VAR -> "Var"
  | TOK_IF -> "If"
  | TOK_ELSE -> "Else"
  | TOK_WHILE -> "While"
  | TOK_READ -> "Read"
  | TOK_O_BRACES -> "("
  | TOK_C_BRACES -> ")"
  | TOK_QUOTE -> "\""
  | TOK_LT -> "<"
  | TOK_LE -> "<="
  | TOK_GT -> ">"
  | TOK_GE -> ">="
  | TOK_EQ -> "=="
  | TOK_ENDL -> "\\n"
  | TOK_STR x -> "\"" ^ x ^ "\""
  | TOK_INT x -> string_of_int x
  | TOK_FLOAT x -> string_of_float x
  | TOK_ID x -> x
  | TOK_EOF -> "EOF"

let num_to_str (n : num) =
  match n with INT x -> string_of_int x | FLOAT x -> string_of_float x

let printable_to_str (p : printable) =
  match p with P_NUM n -> num_to_str n | P_STRING s -> "\"" ^ s ^ "\""

let op_to_str (op : op) =
  match op with ADD -> "+" | SUB -> "-" | MUL -> "*" | DIV -> "/"

let comp_to_str (comp : comp) =
  match comp with LT -> "<" | LE -> "<=" | GT -> ">" | GE -> ">=" | EQ -> "=="

let instr_to_str (instr : instr) =
  match instr with
  | PRINT p -> "Print " ^ printable_to_str p
  | READ x -> "Read " ^ x
  | IF (_a, _b, _c) -> "If"