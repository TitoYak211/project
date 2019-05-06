
all: evaluation expr tests

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

tests: test.ml
	ocamlbuild -use-ocamlfind tests.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

clean:
	rm -rf _build *.byte