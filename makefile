
all: evaluation expr tests

evaluation: evaluation.ml
	ocamlbuild -use-ocamlfind evaluation.byte

tests: tests.ml
	ocamlbuild -use-ocamlfind tests.byte

expr: expr.ml
	ocamlbuild -use-ocamlfind expr.byte

clean:
	rm -rf _build *.byte