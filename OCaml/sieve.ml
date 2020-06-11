(* Euler's sieve

   We maintain a linked list "next" of the numbers not yet removed
   i.e. next.(n) is the next element of the list. The lost only
   contains the odd numbers, to save space, 2n+1 being stored at index n.

   When a new prime p is discovered (when "loop p" starts), we cross
   out the multiple n*p of p for all n still in the list (using
   function "sieve").  To cross out a number x, we simply negate
   next.(x), because x still need to be considered to build the multiple
   x*p. When sieve will reach x later, it will erase it from the list
   (having next skip over it).  *)
let euler_sieve limit =
  let next = Array.init ((limit + 1) / 2) (fun i -> 2*i+3) in
  let get i = next.(i / 2) in
  let rmv i = let v = next.(i / 2) in next.(i / 2) <- - v in
  let set i v = next.(i / 2) <- v in
  let rec loop p =
    let rec sieve prev n = (* n is an element of the list *)
      let np = n * p in
      if np <= limit then begin
          rmv np;
          let n' = get n in
          if n' < 0 then begin set prev (-n'); sieve prev (-n') end
          else sieve n n'
        end in
    sieve (-1) p;
    if p * p <= limit then loop (get p) in
  loop 3;
  let rec count acc prev n =
    if n <= limit then
      let n' = get n in
      if n' < 0 then begin set prev (-n'); count acc prev (-n') end
      else begin next.(acc) <- n; count (acc+1) n n' end
    else Array.sub next 0 acc in
  next.(0) <- 2;
  count 1 (-1) 3

let a = euler_sieve 100
let () = assert (Array.length a = 25)
