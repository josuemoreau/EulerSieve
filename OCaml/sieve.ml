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
  let rmv i = let v = next.(i / 2) in if v > 0 then next.(i / 2) <- - v in
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
      else count (acc+1) n n'
    else acc in
  let count = count 1 (-1) 3 in
  let p = Array.make count 2 in
  let rec fill i n =
    if i = count then assert (n > limit)
    else begin p.(i) <- n; fill (i + 1) (get n) end in
  fill 1 3;
  p

let a = euler_sieve 100
