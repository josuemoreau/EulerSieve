
all: extract
	./extract 100
	time ./extract 20000000

extract: extract.ml test.ml
	ocamlfind ocamlopt -package zarith -linkpkg $^ -o $@

extract.ml: euler_sieve_V2
	why3 extract -D ocaml64 -L . $^.EulerSieveImpl -o $@

