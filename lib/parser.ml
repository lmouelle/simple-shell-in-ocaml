open Angstrom

(*
 shell language: pipeline? \n
 pipeline: command ['|' command]*
 command: word[[\s]+ word]*
 word: token_char+
 token_char: all characters where is_token_char char = true 
*)

type command = {
   executable: string;
   args: string list;
}

let is_whitespace = (function ' ' | '\t' | '\n' -> true | _ -> false)

let is_token_char = function
   | '|' -> false
   | c when is_whitespace c -> false
   | _ -> true

let word_parser = take_while1 is_token_char <?> "failed to parse word"

let whitespace_dropping_parser = skip_while is_whitespace

let command_parser = sep_by1 whitespace_dropping_parser word_parser 
<?> "Failed to parse command" 
>>= function
| [] -> fail "Empty command"
| executable :: args -> return {executable; args}

let pipeline_parser = sep_by1 (whitespace_dropping_parser *> char '|' <* whitespace_dropping_parser) command_parser

let no_op_parser = whitespace_dropping_parser >>| fun _ -> []

let shell_parser = pipeline_parser <|> no_op_parser

(* We really should modify the parser to accept trailing and preceding whitespace
   instead of just trimming the string. But this is easier for now. *)
let parse ln = parse_string ~consume:All shell_parser (String.trim ln)