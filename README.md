# Simple Ocaml Shell

This is an aborted experiment, playing with Angstrom parser library in OCaml. 
the goal was to write a simple shell. It kind of works but is incomplete.
Biggest issue is that using pipes of any kind in interactive mode/repl results in
the shell exiting. Something about doing a basic `ls foo | grep bar` causes EOF exception
to be thrown. At some point I'll revisit this and figure out why EOF is blocking me, and complete the features described in the grammar. For now other projects have my interest.