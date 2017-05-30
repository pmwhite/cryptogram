hello.byte: hello.ml
	ocamlbuild hello.byte -use-ocamlfind -package core -tag thread

hello.d.byte: hello.ml
	ocamlbuild hello.d.byte -use-ocamlfind -package core -tag thread

hello.native: hello.ml
	ocamlbuild hello.native -use-ocamlfind -package core -tag thread

hello.profile: hello.ml
	ocamlbuild hello.byte -use-ocamlfind -package core -tag thread -ocamlc ocamlcp

.PHONY: run runnative debug
run: hello.byte
	./hello.byte words.txt test-cryptos/1.txt

runnative: hello.native
	./hello.native words.txt test-cryptos/1.txt
