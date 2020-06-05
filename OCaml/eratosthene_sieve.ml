let eratosthene_sieve max =
  let primes = Array.make (max + 1) true in
  primes.(0) <- false;
  primes.(1) <- false;
  for n = 2 to max do
    let i = ref 2 in
    while n * !i <= max do
      primes.(n * !i) <- false;
      incr i
    done;
  done;
  let j = ref 0 in
  List.rev (Array.fold_left (fun acc x ->
    incr j; if x then (!j - 1) :: acc else acc) [] primes)
