(* Gahhh I really want to do this with angstrom but it is hard to understand!
   I could go back to using ocamllex/menhir instead, I did thatin college.
   Way more resources for learning and understanding it. IOr hand rol the parser*)

type token = Word of string | Pipe

(* Whitespace* word whitespace+ *)

let rec lex sourcecode position =
  let peek () = String.get sourcecode (position + 1) in
  let advance () = lex sourcecode (position + 1) in

  let pars