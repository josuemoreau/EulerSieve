(*
   Première version
   ----------------

   On a une liste chainée dans un tableau contenant deux entiers dans chaque case.
   A chaque index on associe son prédécesseur et son successeur.
   On va parcourir du début à la fin ce tableau et effacer pour chaque nombre
   lu (qui est alors premier), on parcours une deuxième fois le tableau en effacant
   les produits n * i pour chaque nombre i > p présent dans la liste chainée.

   Pour l'effacement, on construit d'abord une liste des nombres à supprimer, puis
   on parcours celle liste en supprimant les nombres aux index spécifiés dans la
   liste.
*)

#use "eratosthene_sieve.ml"

let mark_for_remove primes n max =
  let rec parse_primes i acc =
    if n * i > max then acc
    else
      begin
        let (_, next_i) = primes.(i) in
        parse_primes next_i ((n * i) :: acc)
      end in
  parse_primes 2 []

let remove_products primes marked =
  List.iter (fun x ->
      let (prec, next) = primes.(x) in
      if prec <> -1 then
        let (prec_p, _) = primes.(prec) in
        primes.(prec) <- (prec_p, next)
      else ();
      if next <> -1 then
        let (_, next_s) = primes.(next) in
        primes.(next) <- (prec, next_s)
      else ();
      primes.(x) <- (-1, -1)) marked

let euler_sieve max =
  let primes = Array.init (max + 1)
      (fun i -> (i - 1, i + 1)) in
  primes.(0) <- (-1, -1);
  primes.(1) <- (-1, -1);
  primes.(max) <- (max - 1, -1);
  let rec aux i =
    if i = -1 then ()
    else
      begin
        let marked = mark_for_remove primes i max in
        remove_products primes marked;
        let (_, next) = primes.(i) in
        aux next
      end in
  aux 2;
  let j = ref (-1) in
  List.rev (Array.fold_left (fun acc x ->
      incr j;
      if x <> (-1, -1) then !j :: acc else acc) [] primes)

let test max =
  let t1 = Sys.time () in
  ignore (eratosthene_sieve max);
  let t2 = Sys.time () in
  ignore (euler_sieve max);
  let t3 = Sys.time () in
  Printf.printf "Crible d'Eratosthene : %f secondes\n Crible d'Euler : %f secondes\n" (t2 -. t1) (t3 -. t2)

let () = test 100000
let () = test 30000000
