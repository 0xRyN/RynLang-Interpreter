open Type

exception Syntax_Error of string

let get_token (word : string) : token =
  match word with
  | "PRINT" -> TOK_PRINT
  | "VAR" -> TOK_VAR
  | "IF" -> TOK_IF
  | "ELSE" -> TOK_ELSE
  | "WHILE" -> TOK_WHILE
  | "READ" -> TOK_READ
  | "{" -> TOK_O_BRACES
  | "}" -> TOK_C_BRACES
  | "\"" -> TOK_QUOTE
  | "<" -> TOK_LT
  | "<=" -> TOK_LE
  | ">" -> TOK_GT
  | ">=" -> TOK_GE
  | "=" -> TOK_EQ
  | num when Str.string_match (Str.regexp "[0-9]+") num 0 ->
      TOK_INT (int_of_string num)
  | num when Str.string_match (Str.regexp "[0-9]+\\.[0-9]+") num 0 ->
      TOK_FLOAT (float_of_string num)
  | str when Str.string_match (Str.regexp {|"[a-zA-Z0-9]+"|}) str 0 ->
      TOK_STR str
  | id when Str.string_match (Str.regexp {|[a-zA-Z_][a-zA-Z0-9_]*|}) id 0 ->
      TOK_ID id
  | _ ->
      Printf.printf "Token %s of length %d not identified.\n" word
        (String.length word);
      raise (Syntax_Error "Lexing Error.")

let line_to_tokens (words_list : string list) : token list =
  TOK_ENDL :: List.rev (List.map get_token words_list)

let lex (filename : string) : tokens =
  let in_ch = open_in filename in
  let rec aux (acc : tokens) =
    try
      let line = input_line in_ch in

      (* Strip off spaces *)
      let words_list = String.split_on_char ' ' line in
      aux (line_to_tokens words_list @ acc)
    with End_of_file -> List.rev (TOK_EOF :: acc)
  in
  aux []
