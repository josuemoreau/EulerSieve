let newton ?(eps=1e-9) f x0 =
  let df x = (f (x +. eps) -. f x) /. eps in
  let rec loop x fx =
    let f'x = df x in
    let x' = x -. fx /. f'x in
    let fx' = f x' in
    if abs_float fx' < eps then x' else loop x' fx'
  in
  loop x0 (f x0)

(* integer square root *)

let isqrt x =
  if x < 0 then invalid_arg "isqrt";
  (* the first guess g0 is the least power of 2 greater or equal to sqrt(x) *)
  let s =
    let s = ref 1 in
    let x1 = ref (x-1) in
    if Sys.word_size = 64 && !x1 > (1 lsl 32 - 1) then begin
      s := !s + 16; x1 := !x1 lsr 32
    end;
    if !x1 > 65535 then begin s := !s + 8; x1 := !x1 lsr 16 end;
    if !x1 > 255 then begin s := !s + 4; x1 := !x1 lsr 8 end;
    if !x1 > 15 then begin s := !s + 2; x1 := !x1 lsr 4 end;
    if !x1 > 3 then incr s;
    !s
  in
  let rec newton g0 g1 =
    if g1 < g0 then newton g1 ((g1 + x/g1) lsr 1) else g0
  in
  if x <= 1 then x else let g0 = 1 lsl s in newton g0 ((g0 + (x lsr s)) lsr 1)

module Bitv = struct
  type t = bool array
  let create = Array.make
  let get = Array.get
  let set = Array.set
end

let first_primes_upto limit =
  let b = Bitv.create (limit + 1) true in
  Bitv.set b 0 false;
  Bitv.set b 1 false;
  for i = 2 to limit / 2 do Bitv.set b (2 * i) false done;
  let rec loop count n =
    if n <= limit then
      if Bitv.get b n then begin (* n is prime *)
	let rec mark i =
	  if i <= limit then begin Bitv.set b i false; mark (i + 2*n) end
	in
	if n <= limit/n then mark (n * n);
	loop (count + 1) (n + 2)
      end else
	loop count (n + 2)
    else
      count

  in
  let count = loop 1 3 in
  let p = Array.make count 0 in
  p.(0) <- 2;
  let rec fill i n =
    if n <= limit then
      if Bitv.get b n then begin p.(i) <- n; fill (i+1) (n+2) end
      else fill i (n+2)
    else begin
      assert (i = count);
      p
    end
  in
  fill 1 3

let segmented_sieve ?(segment_size=32768) limit f =
  let primes = first_primes_upto (isqrt limit) in
  let next = Array.make (Array.length primes) 0 in
  let segment = Bytes.make segment_size '1' in
  let rec loop_segments ~nextp ~nextn ~low =
    if low <= limit then begin
      Bytes.fill segment 0 segment_size '1';
      let high = min (low + segment_size - 1) limit in
      let rec find_nextp nextp =
        if nextp = Array.length primes then nextp else
        let p = primes.(nextp) in
        let p2 = p * p in
        if p2 <= high
        then begin next.(nextp) <- p2 - low; find_nextp (nextp + 1) end
        else nextp in
      let nextp = find_nextp nextp in
      let rec loop_prime i =
        if i < nextp then begin
          let inc = 2 * primes.(i) in
          let rec sieve j =
            if j < segment_size
            then begin Bytes.unsafe_set segment j '0'; sieve (j + inc) end
            else next.(i) <- j - segment_size in
          sieve next.(i);
          loop_prime (i + 1)
        end in
      loop_prime 1;
      let rec iter_primes n =
        if n <= high then begin
          if Bytes.unsafe_get segment (n - low) == '1' then f n;
          iter_primes (n + 2)
        end else
          loop_segments ~nextp ~nextn:n ~low:(low + segment_size) in
      iter_primes nextn
    end
  in
  if limit >= 2 then f 2;
  loop_segments ~nextp:0 ~nextn:3 ~low:0

let limit = int_of_string Sys.argv.(1)
let count = ref 0
let () = segmented_sieve limit (fun _ -> incr count)
let () = Format.printf "%d primes@." !count
