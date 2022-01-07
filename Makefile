.PHONY = all,clean

all:
	rm -f *.byte
	ocamlbuild -use-ocamlfind -cflag -g -lflag -g oshell.byte

clean:
	ocamlbuild -clean