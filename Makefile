hello.byte: hello.ml
	ocamlbuild hello.byte -use-ocamlfind -package core -tag thread

.PHONY: run
run: hello.byte
	./hello.byte words.txt test-cryptos/1.txt
