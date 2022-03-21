open Angstrom

(*
 shell language: command? \n
 command: SimpleCommand | Pipeline | Redirection
 redirection: pipeline | simple_command > filename
 pipeline: simple_command ['|' simple_command]+
 simple command: word[[\s]+ word]*
 word: token_char+
 token_char: all characters where is_token_char char = true 
*)

type simple_command = { executable : string; args : string list }

type command =
  | SimpleCommand of simple_command
  | Pipeline of simple_command list
  | Redirection of (command * string)

let is_whitespace = function ' ' | '\t' | '\n' -> true | _ -> false

let is_token_char = function
  | '|' | '>' -> false
  | c when is_whitespace c -> false
  | _ -> true

let word_parser = take_while1 is_token_char <?> "failed to parse word"
let whitespace_dropping_parser = skip_while is_whitespace

let simple_command_parser =
  sep_by1 whitespace_dropping_parser word_parser <* whitespace_dropping_parser <?> "Failed to parse command"
  >>= function
  | [] -> fail "Empty command"
  | executable :: args -> return { executable; args }

let pipeline_parser : command t =
  sep_by1
    (whitespace_dropping_parser *> char '|' <* whitespace_dropping_parser)
    simple_command_parser
  >>= function
  | [] -> fail "Cannot derive pipeline from empty list"
  | [_] -> fail @@ "Cannot derive pipeline from single element"
  | commands -> return (Pipeline commands)

let promoted_simple_command_parser = simple_command_parser >>= fun simplcmd -> return (SimpleCommand simplcmd)

let redirection_parser =
  (* This promoted_simple_command_parser is needed to wrap simple_command into SimpleCommand. *)

  pipeline_parser <|> promoted_simple_command_parser <* whitespace_dropping_parser <* char '>' *> whitespace_dropping_parser
  >>= fun cmd -> word_parser
  >>= fun filename -> return (Redirection (cmd, filename))


let no_op_parser =
  whitespace_dropping_parser >>| fun _ ->
  SimpleCommand { executable = ""; args = [] }

let shell_parser = pipeline_parser <|> redirection_parser <|> promoted_simple_command_parser <|> no_op_parser

(* We really should modify the parser to accept trailing and preceding whitespace
   instead of just trimming the string. But this is easier for now. *)
let parse ln = parse_string ~consume:All shell_parser (String.trim ln)