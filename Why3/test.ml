
let n = Z.of_string Sys.argv.(1)

let primes = Extract.euler_sieve n

let () = Format.printf "%d prime numbers@." (Array.length primes)
