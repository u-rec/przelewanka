(*Autor: Jerzy Denisiewicz
Recenzent: Patryk Jędrzejczak *)


(*algorytm euklidesa*)
let rec euk a b =
  if a mod b = 0 then b else euk b (a mod b)

exception Impossible

(*sprawdzenie, czy szukany stan jest możliwy do wykonania*)
(*jedna ze szklanek pusta lub pełna; ilość wody w każdej podzielna przez
NWD pojemności wszystkich szklanek*)
let check tab =
  let nwd = Array.fold_left (fun a (x, _) ->
    if x <> 0 then euk a x else a) 0 tab
  in
  if nwd = 0 then true else
  try Array.fold_left (fun a (x, y) ->
    if y mod nwd <> 0 then raise Impossible
    else if y = 0 || y = x then true else a) false tab
  with Impossible -> false

let przelewanka tab =
  if not (check tab) then -1
  else
    let n = Array.length tab in
    let que = Queue.create ()
    and visited = Hashtbl.create 100000
    and xtab = Array.init n (fun i -> fst tab.(i))
    and ytab = Array.init n (fun i -> snd tab.(i))
    and sol = ref (-1)
    in
    let push new_state steps =
      if not (Hashtbl.mem visited new_state) then begin
        Queue.add (new_state, steps + 1) que;
        Hashtbl.add visited new_state ()
      end
    in
    Queue.add (Array.make n 0, 0) que;
    Hashtbl.add visited (Array.make n 0) ();
    while not (Queue.is_empty que) do
      let actual, steps = Queue.pop que in
      if actual = ytab then begin
        sol := steps;
        Queue.clear que
      end else begin
        for i = 0 to n - 1 do
        (*uzupełnianie niepełnej szklanki*)
          if actual.(i) <> xtab.(i) then begin
            let new_state = Array.copy actual in
            new_state.(i) <- xtab.(i);
            push new_state steps
          end;
        (*opróżnianie niepustej szklanki*)
          if actual.(i) <> 0 then begin
            let new_state = Array.copy actual in
            new_state.(i) <- 0;
            push new_state steps
          end;
        (*wszystkie możliwości przelania z niepustej do niepełnej szklanki*)
          for j = 0 to n - 1 do
            if actual.(j) <> xtab.(j) && actual.(i) <> 0 && i <> j then begin
              let new_state = Array.copy actual in
              let old_i = actual.(i) and old_j = actual.(j) in
              new_state.(j) <- min (old_i + old_j) xtab.(j);
              new_state.(i) <- max 0 (old_i - xtab.(j) + old_j);
              push new_state steps
            end
          done
        done
      end
    done;
    !sol
