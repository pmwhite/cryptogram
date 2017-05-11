hello.byte:
	ocamlbuild hello.byte -use-ocamlfind -package core -tag thread

.PHONY: run
run: hello.byte
	./hello.byte wordcounts.txt
