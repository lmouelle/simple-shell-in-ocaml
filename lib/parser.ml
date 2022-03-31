open Angstrom

(*
commandline ::= list (list_op)?

list ::=  conditional (list_op conditional)*
list_op ::= ";" | "&"

conditional ::= pipeline (conditional_op pipeline)*
conditional_op ::= "&&" | "||"

pipeline ::=  command ('|' command)*

command  ::=  (word | redirection)+

redirection  ::=  redirectionop filename
redirectionop  ::=  "<"  |  ">"  |  "2>"
*)

type redirection =
  | StderrRedirect of string
  | StdinRedirect of string
  | StdoutRedirect of string
  
type command = [`Redirection of redirection | `Word of string] list
type pipeline = command list
type conditional = Pipeline of pipeline | Or of (conditional * conditional) | And of (conditional * conditional)
type shell_list = Conditional of conditional |Foreground of (shell_list * shell_list) | Background of (shell_list * shell_list)
type commandline = CmdlineForeground of shell_list | CmdlineBackground of shell_list | List of shell_list

let is_seperating_whitespace = function ' ' | '\t' -> true | _ -> false
let is_word_char = function ' ' | '\t' | '\r' | '\n' | '|' | '$' | '&' | ';' | '>' | '<' -> false | _ -> true
let whitespace_dropping_parser = skip_while is_seperating_whitespace

let word_parser =
  whitespace_dropping_parser *> 
  take_while1 is_word_char
  <* whitespace_dropping_parser 
  >>= fun word -> return @@ `Word word

let filename_parser = whitespace_dropping_parser *> take_while1 (fun c -> not @@ is_seperating_whitespace c) <* whitespace_dropping_parser

let redirection_parser =
  whitespace_dropping_parser *> (string "<" <|> string ">" <|> string "2>") <* whitespace_dropping_parser
  >>= fun operator -> filename_parser 
  >>= fun filename ->
  match operator with
  | "<" -> return @@ `Redirection (StdinRedirect filename)
  | ">" -> return @@ `Redirection (StdoutRedirect filename)
  | "2>" -> return @@ `Redirection (StderrRedirect filename)
  | _ -> fail "Unexpected redirection character found"
  <* whitespace_dropping_parser


let command_parser : command t = many1 (word_parser <|> redirection_parser) 

let pipeline_parser : pipeline t = sep_by1 (char '|') command_parser

let chainl1 e op =
  let rec go acc =
    (lift2 (fun f x -> f acc x) op e >>= go) <|> return acc in
  e >>= fun init -> go init

let and_parser = string "&&" *> return (fun lhs rhs -> And (lhs, rhs))

let or_parser = string "||" *> return (fun lhs rhs -> Or (lhs, rhs))

let conditional_parser = 
  let initialvalue : conditional t = pipeline_parser >>= fun p -> return (Pipeline p) in
  chainl1 initialvalue (and_parser <|> or_parser)

let foreground_parser = string ";" *> return (fun lhs rhs -> Foreground (lhs, rhs))

let background_parser = string "&" *> return (fun lhs rhs -> Background (lhs, rhs))
  
let list_parser =
  let initialvalue : shell_list t = conditional_parser >>= fun c -> return (Conditional c) in
  chainl1 initialvalue (background_parser <|> foreground_parser)

let commandline_parser =
  list_parser >>= fun lst -> 
  (char ';' <|> char '&' >>= fun operator ->
  match operator with
  | ';' -> return (CmdlineForeground lst)
  | '&' -> return (CmdlineBackground lst)
  | _ -> failwith "unexpected operator") <|> return @@ List lst

let parse_with str parser = parse_string ~consume:All parser str

let parse s = parse_with s commandline_parser