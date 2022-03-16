open Lib.Parser

let oshell_cd args = 
  match args with 
  | [] -> Printf.eprintf "Expected an argument for change directory"; 1
  | arg :: [] -> Unix.chdir arg; 0
  | _ -> Printf.eprintf "Received too many args for change directory"; 1
;;

let oshell_help () = 
  Printf.printf "Oshell: a simple ocaml shell.\nBuilt ins: cd, quit/exit\n"
;;

let builtin_functions = 
  [("quit", fun _ -> ignore @@ exit 0; 0);
   ("exit", fun _ -> ignore @@ exit 0; 0);
   ("help", fun _ -> oshell_help (); 0);
   ("cd", fun args -> oshell_cd args)]
;;


let waitforprocess pid =
  let (_, status) = Unix.waitpid [Unix.WUNTRACED] pid in
  match status with
  | Unix.WEXITED exitcode -> exitcode
  | Unix.WSIGNALED exitcode -> exitcode
  | _ -> failwith "Stopped processes unimplemented"
in

let rec exec_pipeline = function
| [] -> 0
| ({executable; args} :: []) -> 
  begin
    match List.assoc_opt executable builtin_functions with
    | Some func -> func args
    | None ->
      begin
        let argv = Array.of_list (executable :: args) in
        let pid = (Unix.fork ()) in
        if pid = 0 then Unix.execvp executable argv
        else if pid < 0 then (Printf.eprintf "Failure forking for process %s" executable; 1)
        else waitforprocess pid
      end
  end
| ({executable; args} :: remainder) ->
    begin
      let fd_in, fd_out = Unix.pipe () in
      let argv = Array.of_list (executable :: args) in
      let pid = Unix.fork () in
      if pid = 0 then
        begin
          Unix.dup2 fd_out Unix.stdout;
          Unix.close fd_out;
          Unix.close fd_in;
          Unix.execvp executable argv
        end
      else if pid < 0 then (Printf.eprintf "Failure forking for process %s" executable; 1)
      else 
        begin
          Unix.dup2 fd_in Unix.stdin;
          Unix.close fd_in;
          Unix.close fd_out;
          exec_pipeline remainder
        end
    end
in

let run (line : string) =
  match parse line with
  | Error msg -> Printf.eprintf "Error parsing commands, %s" msg; 1
  | Ok commands -> exec_pipeline commands
in

let run_file filename = 
  let chan = open_in filename in
  try
    while true; do
      let ln = input_line chan in
      ignore (run ln);
    done; 
  with End_of_file ->
    close_in chan;
in

let run_prompt () =
  try
    while true; do
      Printf.printf "> ";
      let ln = read_line () in
      ignore (run ln);
    done; 
  with End_of_file -> ()
in

(* Top level *)
if Array.length Sys.argv > 2 then
  begin
   Printf.printf "Usage: oshell [script]";
   exit 64
  end
else if Array.length Sys.argv == 2 then
  begin
    let filename = Array.get Sys.argv 0 in
    run_file filename
  end
else
  begin
    run_prompt ()
  end