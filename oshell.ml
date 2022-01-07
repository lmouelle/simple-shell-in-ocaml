let rec waitforprocess pid =
  let (_, status) = Unix.waitpid [Unix.WUNTRACED] pid in
  match status with
  | Unix.WEXITED exitcode -> exitcode
  | Unix.WSIGNALED exitcode -> exitcode
  | _ -> failwith "Stopped processes unimplemented"
in

let run (line : string) =
  let tokens = Str.split (Str.regexp " +") line in
  match tokens with 
  | [] -> 0 (* Empty input is a no-op for now *)
  | cmd :: args -> 
    begin
      let argv = (Array.of_list args) in
      let pid = (Unix.fork ()) in
      if pid == 0 then Unix.execvp cmd argv
      else if pid < 0 then (Printf.eprintf "Failure forking for process %s" cmd; 1)
      else waitforprocess pid
    end
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
  with End_of_file ->
    print_string "Leaving shell\n";
in

(* Top level *)
if Array.length Sys.argv > 2 then
  begin
   Printf.printf "Usage: oshell [script]";
   exit 64
  end
else if Array.length Sys.argv == 2 then
  begin
    Printf.printf "File mode\n";
    let filename = Array.get Sys.argv 0 in
    run_file filename
  end
else
  begin
    Printf.printf "Prompt mode\n";
    run_prompt ()
  end