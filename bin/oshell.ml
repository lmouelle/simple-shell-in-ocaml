open Lib.Parser
open Lib.Process

let run (line : string) =
  match parse line with
  | Error msg ->
      Printf.eprintf "Error parsing commands, %s" msg;
      1
  | Ok commands -> exec commands

let run_file filename =
  let chan = open_in filename in
  try
    while true do
      let ln = input_line chan in
      ignore (run ln)
    done
  with End_of_file -> close_in chan

let run_prompt () =
  try
    while true do
      Printf.printf "> ";
      let ln = read_line () in
      ignore (run ln)
    done
  with End_of_file -> ()
;;

(* Top level *)
if Array.length Sys.argv > 2 then (
  Printf.printf "Usage: oshell [script]";
  exit 64)
else if Array.length Sys.argv == 2 then
  let filename = Array.get Sys.argv 1 in
  run_file filename
else run_prompt ()
