Euler's sieve
================

A formally verified Euler's Sieve, written in OCaml.
The proof was made with [Why3](http://why3.lri.fr/).

In the OCaml folder, there are some files that are not related with the proof.
In particular, there are the file
[eratosthene_sieve.ml](/OCaml/eratosthene_sieve.ml), which is the
implementation of the sieve of Eratosthene used for benchmarks,
and [seg.ml](/OCaml/seg.ml) an implementation of the segmented sieve of
Eratosthene.

The proof is in the folder Why3, in the file [euler_sieve.mlw]().
In the same folder, there is also the Why3 session in
[euler_sieve]() and the extracted code from the proof [extract.ml]().
