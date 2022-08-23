open Type

exception Parsing_Error of string

let error (err : string) (tok : token) =
  Printf.printf "<Parsing Error>: at token %s\n" (Printer.token_to_str tok);
  raise (Parsing_Error err)

let parse_print (input : tokens) : instr * tokens =
  match input with
  | printable :: endl :: tl -> (
      if endl <> TOK_ENDL then error "No newline at end of print." endl
      else
        match printable with
        | TOK_INT x -> (PRINT (P_NUM (INT x)), tl)
        | TOK_FLOAT x -> (PRINT (P_NUM (FLOAT x)), tl)
        | TOK_STR x -> (PRINT (P_STRING x), tl)
        | _ -> error "Trying to print a non printable." printable)
  | _ -> error "Unknown err" (List.hd input)

let parse (input : tokens) : block =
  let rec parse_instr (acc : block) (rest : tokens) =
    match rest with
    | hd :: tl -> (
        match hd with
        | TOK_PRINT ->
            let print, rest = parse_print tl in
            parse_instr (print :: acc) rest
        | TOK_EOF -> acc
        | _ -> error "Instruction unknown" hd)
    | [] -> acc
  in
  parse_instr [] input
