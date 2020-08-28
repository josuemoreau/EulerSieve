
all: extract
	./extract 100
	time ./extract 200000000

extract: extract.ml test.ml
	ocamlopt -inline 10000 $^ -o $@

extract.ml: euler_sieve.mlw
	why3 extract -D ocaml64 -L . euler_sieve.EulerSieveImpl -o $@

