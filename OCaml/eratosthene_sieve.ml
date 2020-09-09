let eratosthene_sieve max =
  let len = (max - 1) / 2 + 1 in
  let primes = Bitv.create len true in
  let get i = Bitv.get primes (i / 2) in
  let set i v = Bitv.set primes (i / 2) v in
  set 1 false;
  let cnt = ref 0 in
  let n = ref 3 in
  while !n <= max do
    if get !n then begin
      incr cnt;
      let i = ref !n in
      while !n * !i <= max do
        set (!n * !i) false;
        i := !i + 2
      done
    end;
    n := !n + 2
  done;
  let j = ref 1 in
  let a = Array.make (!cnt + 1) 0 in
  a.(0) <- 2;
  Bitv.iteri (fun i x ->
      if x then begin a.(!j) <- 2 * i + 1; incr j end) primes;
  a

let () =
  let max = int_of_string Sys.argv.(1) in
  let a = eratosthene_sieve max in
  Format.printf "Il y a %d nombres premiers entre 2 et %d.@."
    (Array.length a) max

(* let () =
 *   let thw = (Gc.stat ()).Gc.top_heap_words in
 *   Format.printf "used %d bytes@." (thw * (Sys.word_size lsr 3)) *)

