open Parser
open Unix

let waitforprocess pid =
  let _, status = waitpid [ Unix.WUNTRACED ] pid in
  match status with
  | Unix.WEXITED exitcode -> exitcode
  | Unix.WSIGNALED exitcode -> exitcode
  | _ -> failwith "Stopped processes unimplemented"
;;

let rec exec = function
| List lst -> exec_list lst
| _ -> failwith "todo"
and exec_list = function
| Conditional c -> exec_cond c
| _ -> failwith "todo"
and exec_cond = function
| Pipeline p -> exec_pipeline p
| _ -> failwith "todo"
and exec_pipeline = function
| command :: _  -> exec_command command
| _ -> failwith "todo"
and exec_redirect = function
| _ -> failwith "todo"
and exec_command = failwith "todo"