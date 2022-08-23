type name = string

type num = INT of int | FLOAT of float

type printable = P_NUM of num | P_STRING of string

type op = ADD | SUB | MUL | DIV

type comp = LT | LE | GT | GE | EQ

type expr = VAR of name | NUM of num | OP of expr * op * expr

type cond = expr * comp * expr

type instr = PRINT of printable | READ of name | IF of cond * block * block

and block = instr list

type ast = block

(* Tokens *)

type token =
  | TOK_PRINT
  | TOK_VAR
  | TOK_IF
  | TOK_ELSE
  | TOK_WHILE
  | TOK_READ
  | TOK_O_BRACES
  | TOK_C_BRACES
  | TOK_QUOTE
  | TOK_LT
  | TOK_LE
  | TOK_GT
  | TOK_GE
  | TOK_EQ
  | TOK_ENDL
  | TOK_STR of string
  | TOK_INT of int
  | TOK_FLOAT of float
  | TOK_ID of name
  | TOK_EOF

type tokens = token list
