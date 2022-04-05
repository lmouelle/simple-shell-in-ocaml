open Parser
open Unix

let waitforprocess pid =
  let _, status = waitpid [ Unix.WUNTRACED ] pid in
  match status with
  | Unix.WEXITED exitcode -> exitcode
  | Unix.WSIGNALED exitcode -> exitcode
  | _ -> failwith "Stopped processes unimplemented"

let rec exec = function List lst -> exec_list lst | _ -> failwith "todo"

and exec_list = function Conditional c -> exec_cond c | _ -> failwith "todo"

and exec_cond = function
  | Pipeline p -> exec_pipeline p
  | Or (lhs, rhs) ->
      let retcode = exec_cond lhs in
      if retcode <> 0 then exec_cond rhs else retcode
  | And (lhs, rhs) ->
      let retcode = exec_cond lhs in
      if retcode <> 0 then retcode else exec_cond rhs

and exec_pipeline = function
  | command :: _ -> exec_command command
  | _ -> failwith "todo"

and exec_command cmd =
  let pid = fork () in
  if pid < 0 then failwith "Fork failure"
  else if pid > 0 then waitforprocess pid
  else
    let err =
      match cmd.outfile with
      | Some (StderrRedirect filename) ->
          Unix.openfile filename [ Unix.O_TRUNC; O_CREAT; Unix.O_WRONLY ] 0o640
      | _ -> Unix.stderr
    in
    let out =
      match cmd.outfile with
      | Some (StdoutRedirect filename) ->
          Unix.openfile filename [ Unix.O_TRUNC; O_CREAT; Unix.O_WRONLY ] 0o640
      | _ -> Unix.stdout
    in
    let input =
      match cmd.outfile with
      | Some (StdinRedirect filename) ->
          Unix.openfile filename [ O_RDONLY ] 0o640
      | _ -> Unix.stdin
    in
    Unix.dup2 out Unix.stdout;
    Unix.dup2 err Unix.stderr;
    Unix.dup2 input Unix.stdin;
    Unix.execvp cmd.executable (Array.of_list (cmd.executable :: cmd.args))
