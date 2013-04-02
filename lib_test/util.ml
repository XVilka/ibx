open Core.Std

(* Slightly modifies the split_n function in core_list.ml in Core. *)
let rev_split_n l_orig n =
  if n <= 0 then
    ([], l_orig)
  else
    let rec loop n l acc =
      if n = 0 then
        (acc, l)
      else
        match l with
        | [] -> (acc, [])
        | h :: t -> loop (n - 1) t (h :: acc)
    in
    loop n l_orig []

let rand_insert l x =
  let front, back = rev_split_n l (Random.int (1 + List.length l)) in
  List.rev_append front (x :: back)
