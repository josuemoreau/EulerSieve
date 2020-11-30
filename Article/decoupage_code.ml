type t = { arr: (int array);  max: int;  max_arr: int; }

(* marque tous les multiples de n encore non marqués et saute par dessus les *)
(* nombres déjà marqués qu'il croise *)
let remove_products (t: t) (n: int) : unit =
  let d = get_max t / n in
  let rec loop (p: int) : unit =
    let next = get_next t p in
    if 0 <= next && next <= get_max t then
      if next <= d then begin
        set_mark t (n * next);
        if get_mark t next then begin
          (* si next était déjà marqué, alors on le retire de la liste chaînée *)
          (* en sautant par dessus *)
          set_next t p (get_next t next);
          loop p
        end else loop next (* sinon on passe à l'entier suivant *)
      end in
  set_mark t (n * n); (* tous les n * i pour i \< n sont déjà marqués *)
  loop n

let euler_sieve (max: int) : int array =
  (* initialement, la liste chaînée contient tous les nombres impairs et pour *)
  (* marquer la fin de la liste chaînée, son dernier entier a pour valeur max + 1 *)
  let t = create max in
  let rec loop (n: int) : unit =
    (* n prend successivement les valeurs des nombres premiers *)
    remove_products t n;
    let nn = get_next t n in
    if nn <= max / nn then loop1 nn in
  if max >= 9 then loop 3;
  ... extraction des nombres premiers ...

(*******************************************************************************)

let set_next (t: t) (i: int) (v: int) : unit = t.arr.(i / 2) <- v
let get_next (t: t) (i: int) : int =
  if t.arr.(i / 2) < 0 then - t.arr.(i / 2)
  else t.arr.(i / 2)

let set_mark (t: t) (i: int) : unit =
  if t.arr.(i / 2) >= 0 then t.arr.(i / 2) <- - t.arr.(i / 2)
let get_mark (t: t) (i: int) : bool = t.arr.(i / 2) < 0

(*******************************************************************************)

let create (max: int) : t =
  let len_arr = (max - 1) / 2 + 1 in
  let arr = Array.make len_arr (-2) in
  for i = 1 to len_arr - 1 do
    arr.(i) <- (if i = len_arr - 1 then max + 1 else 2 * i + 3)
  done;
  { arr = arr; max = max; max_arr = (max - 1) / 2 }

(*******************************************************************************)

let get_max (t: t) : int = t.max

(*******************************************************************************)

let cnt = ref 1 in
let p = ref 1 in t.arr.(0) <- 2;
while 2 * !p + 1 <= max do
  let next = t.arr.(!p) / 2 in
  (* on élimine les derniers multiples et on écrase le début de la liste chaînée *)
  if next <= t.max_arr then
    if t.arr.(next) < 0 then t.arr.(!p) <- - t.arr.(next)
    else begin
      t.arr.(!cnt) <- 2 * !p + 1;
      cnt := !cnt + 1;
      p := next end
  else begin
    t.arr.(!cnt) <- 2 * !p + 1;
    cnt := !cnt + 1;
    p := t.max_arr + 1 end
done;
Array.sub t.arr 0 !cnt (* extrait les nombres premiers écrits au début de t.arr *)
