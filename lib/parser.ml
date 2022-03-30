open Angstrom

(*

let list_op = ";" | "&"

commandline ::= list (list_op list)*

list ::=  conditional (list_op conditional)*

let conditional_op = "&&" | "||"

conditional ::= pipeline (conditional_op pipeline)*

pipeline ::=  command ('|' command)*

command  ::=  (word | redirection)+

redirection  ::=  redirectionop filename
redirectionop  ::=  "<"  |  ">"  |  "2>"

*)

type redirection =
  | StderrRedirect of string
  | StdinRedirect of string
  | StdoutRedirect of string

and command = [`Word of string | `Redirection of redirection] list

and pipeline = Commands of command list

and conditional =
  | Pipeline of pipeline
  | And of (conditional * pipeline)
  | Or of (conditional * pipeline)

and oshell_list =
  | Conditional of conditional
  | Background of (oshell_list * conditional)
  | Foreground of (oshell_list * conditional)

and commandline =
  | List of oshell_list
  | EndsWithBackground of oshell_list
  | EndsWithForeground of oshell_list

type ast = 
| Word of string
| Redirection of redirection
| Command of command
| Pipeline of pipeline
| Conditional of conditional
| List of oshell_list
| Commandline of commandline

let is_seperating_whitespace = function ' ' | '\t' -> true | _ -> false
let is_word_char = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
let whitespace_dropping_parser = skip_while is_seperating_whitespace

let word_parser =
  whitespace_dropping_parser *> take_while1 is_word_char
  <* whitespace_dropping_parser >>= fun word -> return @@ Word word

let filename_parser = whitespace_dropping_parser *> take_while1 (fun c -> not @@ is_seperating_whitespace c) <* whitespace_dropping_parser

let redirection_parser =
  whitespace_dropping_parser *> (string "<" <|> string ">" <|> string "2>") <* whitespace_dropping_parser
  >>= fun operator -> filename_parser 
  >>= fun filename ->
  match operator with
  | "<" -> return @@ Redirection (StdinRedirect filename)
  | ">" -> return @@ Redirection (StdoutRedirect filename)
  | "2>" -> return @@ Redirection (StderrRedirect filename)
  | _ -> fail "Unexpected redirection character found"
  <* whitespace_dropping_parser

let command_parser = 
  many1 (word_parser <|> redirection_parser) 
  >>= fun (elements) ->
    let commandlist : command = List.map (function Word w -> `Word w | Redirection r -> `Redirection r | _ -> failwith "Unexpected elem type") elements in
    return @@ Command commandlist

let pipeline_parser = sep_by1 (char '|') command_parser >>= fun (cmds) -> 
  let commandlist = List.map (function Command c -> c | _ -> failwith "Unexpected type") cmds in
  let commandlist2 = Commands commandlist in
  return @@ Pipeline commandlist2

let parse_with str parser = parse_string ~consume:All parser str