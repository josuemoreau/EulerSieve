
let n = int_of_string Sys.argv.(1)

let primes = Extract.euler_sieve n

let () = Format.printf "%d prime numbers@." (Array.length primes)

let () =
  let thw = (Gc.stat ()).Gc.top_heap_words in
  Format.printf "used %d bytes@." (thw * (Sys.word_size lsr 3))
