hello.byte: hello.ml
	ocamlbuild hello.byte -use-ocamlfind -package core -tag thread

hello.native: hello.ml
	ocamlbuild hello.native -use-ocamlfind -package core -tag thread

.PHONY: run
run: hello.byte
	./hello.byte words.txt test-cryptos/1.txt

.PHONY: runnative
runnative: hello.native
	./hello.native words.txt test-cryptos/1.txt
