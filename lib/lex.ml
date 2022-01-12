open Angstrom

type token = Word of string | Pipe | Semicolon

(* Whitespace, word, whitespace, *)

let is_space = (function ' ' | '\t' -> true | _ -> false ) 

let parse_word = skip_while is_space *> take_till (fun c -> not @@ is_space c) <* skip_while is_space
  >>= fun lexeme -> return (Word lexeme)


let parse_pipe = char '|'
let parse_end_command = char '|' <|> char ';'