(*
   Deuxième version
   ----------------

   Parce que la première version n'est quand même pas très propre...
   Et surtout qu'elle prend pas mal de mémoire !
   Enfin ces deux problèmes sont un peu liés...
*)

#use "eratosthene_sieve.ml"

let mark marked n = marked.(n) <- true

let rec remove_products nexts marked max n p =
  let next = if p = -1 then n else nexts.(p) in
  if next = -1 then () (* Si la case p n'a pas de suivante *)
  else
      if n * next > max then ()
      else
        begin
          mark marked (n * next);
          if p <> -1 && marked.(next) then
            (nexts.(p) <- nexts.(next);
             remove_products nexts marked max n p)
          else
            remove_products nexts marked max n next
        end

let euler_sieve max =
  let nexts = Array.init (max + 1)
      (fun i -> i + 1) in
  let marked = Array.make (max + 1) false in
  marked.(0) <- true;
  marked.(1) <- true;
  nexts.(max) <- -1;
  let rec aux n =
    if n = -1 then ()
    else
      (remove_products nexts marked max n (-1);
       aux nexts.(n)) in
  aux 2;
  (*let j = ref (-1) in
  List.rev (Array.fold_left (fun acc x ->
      incr j;
      if x then acc else !j :: acc) [] marked)*)
  let cnt = ref 0 in
  for i = 0 to max do
    if not marked.(i) then incr cnt
  done;
  let primes = Array.make !cnt 0 in
  let p = ref 0 in
  for i = 0 to max do
    if not marked.(i) then begin
      primes.(!p) <- i;
      incr p
    end
  done;
  primes

let test max =
  let t1 = Sys.time () in
  ignore (eratosthene_sieve max);
  let t2 = Sys.time () in
  ignore (euler_sieve max);
  let t3 = Sys.time () in
  Printf.printf "Crible d'Eratosthene : %f secondes\nCrible d'Euler : %f secondes\n" (t2 -. t1) (t3 -. t2)

let () = test 100000
let () = test 30000000

let l1 = euler_sieve 10000
let l2 = eratosthene_sieve 10000
let b = Array.to_list l1 = l2
