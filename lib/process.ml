open Parser
open Unix

let rec waitforprocess retcode =
  try
    match wait () with
    | _, WEXITED n -> waitforprocess (retcode lor n)
    | _, _ -> waitforprocess 127
  with Unix_error (ECHILD, _, _) -> retcode

let rec exec_pipeline = function
  | [] -> 0
  | [ { executable; args } ] ->
      let argv = Array.of_list (executable :: args) in
      let pid = fork () in
      if pid = 0 then execvp executable argv
      else if pid < 0 then (
        Printf.eprintf "Failure forking for process %s" executable;
        1)
      else waitforprocess pid
  | { executable; args } :: remainder ->
      let fd_in, fd_out = pipe () in
      let argv = Array.of_list (executable :: args) in
      let pid = fork () in
      if pid = 0 then (
        dup2 fd_out stdout;
        close fd_out;
        close fd_in;
        execvp executable argv)
      else if pid < 0 then (
        Printf.eprintf "Failure forking for process %s" executable;
        1)
      else (
        dup2 fd_in stdin;
        close fd_in;
        close fd_out;
        exec_pipeline remainder)
