type t = {
  arr: (int array);
  max: int;
  max_arr: int;
  }

let create (max1: int) : t =
  let len_arr = (max1 - 1) / 2 + 1 in
  let arr1 = Array.make len_arr (-2) in
  (let o = len_arr - 1 in let o1 = 1 in
   for i = o1 to o do
     arr1.(i) <- (if i = len_arr - 1 then max1 + 1 else 2 * i + 3)
     done);
  { arr = arr1; max = max1; max_arr = (max1 - 1) / 2 }

let set_next (t1: t) (i1: int) (v: int) : unit = (t1.arr).(i1 / 2) <- v

let get_next (t1: t) (i1: int) : int =
  let x = i1 / 2 in if (t1.arr).(x) < 0 then - (t1.arr).(x) else (t1.arr).(x)

let set_mark (t1: t) (i1: int) : unit =
  let x = i1 / 2 in if (t1.arr).(x) >= 0 then (t1.arr).(x) <- - (t1.arr).(x)

let get_mark (t1: t) (i1: int) : bool = (t1.arr).(i1 / 2) < 0

let get_max (t1: t) : int = t1.max

open Format

let pp_array f a =
  fprintf f "| ";
  for i = 0 to Array.length a - 1 do
    fprintf f "%3d | " (2 * i + 1)
  done;
  fprintf f "\n| ";
  Array.iter (fun x -> fprintf f "%3d | " x) a;
  fprintf f "\n"

let remove_products (t1: t) (n: int) : unit =
  let d = get_max t1 / n in
  let rec loop (p: int) : unit =
    let next = get_next t1 p in
    if 0 <= next && next <= get_max t1
    then begin
      if next <= d
      then begin
        set_mark t1 (n * next);
        printf "%a@." pp_array t1.arr;
        ignore (read_line ());
        if get_mark t1 next
        then begin set_next t1 p (get_next t1 next); loop p end
        else loop next
      end end in
  set_mark t1 (n * n);
  printf "%a@." pp_array t1.arr;
  ignore (read_line ());
  loop n

let euler_sieve (max1: int) : (int array) =
  let t1 = create max1 in
  let rec loop1 (n: int) : unit =
    remove_products t1 n;
    let nn = get_next t1 n in if nn <= max1 / nn then loop1 nn in
  printf "%a@." pp_array t1.arr;
  ignore (read_line ());
  if max1 >= 9 then loop1 3;
  let cnt = ref 1 in
  let p = ref 1 in
  (t1.arr).(0) <- 2;
  while 2 * !p + 1 <= max1 do
    let next = (t1.arr).(!p) / 2 in
    if next <= t1.max_arr
    then
      if (t1.arr).(next) < 0
      then (t1.arr).(!p) <- - (t1.arr).(next)
      else
        begin (t1.arr).(!cnt) <- 2 * !p + 1; cnt := !cnt + 1; p := next end
    else
      begin
        (t1.arr).(!cnt) <- 2 * !p + 1; cnt := !cnt + 1; p := t1.max_arr + 1
      end
  done;
  Array.sub t1.arr 0 !cnt

let _ = euler_sieve (int_of_string Sys.argv.(1))
