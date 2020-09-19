Euler's sieve
================

A formally verified Euler's Sieve, written in OCaml.
The proof was made with [Why3](http://why3.lri.fr/).

The proof is in the folder Why3, in the file
[euler_sieve.mlw](/Why3/euler_sieve.mlw).
In the same folder, there is also the Why3 session in
[euler_sieve](/Why3/euler_sieve/).
To get the extracted code from Why3, install Why3 then run the Makefile,
the code will be extracted in the file ```extract.ml```.
This extracted code is also written below.

In the OCaml folder, there are some files that are not related with the proof.
In particular, there are the file
[eratosthene_sieve.ml](/OCaml/eratosthene_sieve.ml), which is the
implementation of the sieve of Eratosthene used for benchmarks,
and [seg.ml](/OCaml/seg.ml) an implementation of the segmented sieve of
Eratosthene.

```ocaml
type t = { arr: int array; max: int; max_arr: int }

let create max =
  let len_arr = (max - 1) / 2 + 1 in
  let arr = Array.make len_arr (-2) in
  for i = 1 to len_arr - 1 do
    arr.(i) <- if i = len_arr - 1 then max + 1 else 2 * i + 3
  done;
  { arr = arr; max = max; max_arr = (max - 1) / 2 }

let set_next t i v = t.arr.(i / 2) <- v
let get_next t i = if t.arr.(i / 2) < 0 then - t.arr.(i / 2) else t.arr.(i / 2)
let set_mark t i = if t.arr.(i / 2) >= 0 then t.arr.(i / 2) <- - t.arr.(i / 2)
let get_mark t i = t.arr.(i / 2) < 0
let get_max t = t.max

let remove_products t n =
  let d = get_max t / n in
  let rec loop (p: int) : unit =
    let next = get_next t p in
    if 0 <= next && next <= get_max t then begin
      if next <= d then begin
        set_mark t (n * next);
        if get_mark t next then begin set_next t p (get_next t next); loop p end
        else loop next
      end end in
  set_mark t (n * n); loop n

let euler_sieve max =
  let t = create max in
  let rec loop n =
    remove_products t n;
    let nn = get_next t n in
    if nn <= max / nn then loop nn in
  if max >= 9 then loop 3;
  let cnt = ref 1 in
  let p = ref 1 in t.arr.(0) <- 2;
  while 2 * !p + 1 <= max do
    let next = t.arr.(!p) / 2 in
    if next <= t.max_arr then
      if t.arr.(next) < 0 then t.arr.(!p) <- - t.arr.(next)
      else begin t.arr.(!cnt) <- 2 * !p + 1; cnt := !cnt + 1; p := next end
    else begin t.arr.(!cnt) <- 2 * !p + 1; cnt := !cnt + 1; p := t.max_arr + 1 end
  done;
  Array.sub t.arr 0 !cnt
```
