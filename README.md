Euler's sieve
================

A formally verified Euler's Sieve, written in OCaml.
The proof was made with [Why3](http://why3.lri.fr/).

The original OCaml code, written before the proof is the
[euler_sieve.ml](/OCaml/euler_sieve.ml)
in the OCaml folder. There are also some other files, namely
[eratosthene_sieve.ml](), which is the implementation of the sieve of Eratosthene
used for tests, and [seg.ml]() an implementation of the segmented sieve of
Eratosthene.

The proof is in the folder Why3, in the file [euler_sieve.mlw]().
In the same folder, there is also the Why3 session in
[euler_sieve]() and the extracted code from the proof [extract.ml]().
