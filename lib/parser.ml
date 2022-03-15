open Angstrom

(* Gahhh I really want to do this with angstrom but it is hard to understand!
   I could go back to using ocamllex/menhir instead, I did thatin college.
   Way more resources for learning and understanding it. IOr hand rol the parser*)

let split_string =  Str.split @@ Str.regexp " +"

type command = {
   executable: string;
   args: string list;
}

let is_whitespace = (function ' ' | '\t' | '\n' -> true | _ -> false)

let is_token_char = function
   | '|' -> false
   | c when is_whitespace c -> false
   | _ -> true

let whitespace_parser = take_while is_whitespace

let pipe_parser = char '|' <* whitespace_parser

let token_parser = take_while1 is_token_char <* whitespace_parser

let command_parser = many token_parser >>= fun tokens ->
   match tokens with
   | [] -> return None
   | executable :: args -> return (Some {executable; args})

let pipeline_parser = sep_by pipe_parser command_parser >>= function
   | [] -> return None
   | possible_commands -> 
      if (List.filter Option.is_none possible_commands) <> []
      then fail "Invalid pipeline list"
      else 
         let commands = List.map Option.get possible_commands in
         return (Some commands)

let parse_line ln =
   Angstrom.parse_string ~consume:All pipeline_parser (String.trim ln)