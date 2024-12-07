let readlines filename =
  let ch = In_channel.open_text filename in
  let lines = In_channel.input_lines ch in
  In_channel.close ch;
  lines
;;

let parse filename =
  let lines = readlines filename in
  List.map
    (fun line ->
      match String.split_on_char ':' line with
      | [ res; vals ] ->
        ( Int64.of_string res
        , String.split_on_char ' ' vals |> List.tl |> List.map Int64.of_string )
      | _ -> invalid_arg "could not parse input")
    lines
;;

let ( + ) = Int64.add
let ( * ) = Int64.mul

let log10_and_mul v =
  if v >= 1_000_000_000L
  then 10_000_000_000L
  else if v >= 100_000_000L
  then 1_000_000_000L
  else if v >= 10_000_000L
  then 100_000_000L
  else if v >= 1_000_000L
  then 10_000_000L
  else if v >= 100_000L
  then 1_000_000L
  else if v >= 10_000L
  then 100_000L
  else if v >= 1_000L
  then 10_000L
  else if v >= 100L
  then 1_000L
  else if v >= 10L
  then 100L
  else 10L
;;

let concat n1 n2 = (n1 * log10_and_mul n2) + n2

let try_eq (res, vals) =
  let rec aux rv cur =
    if cur > res
    then false
    else (
      match rv with
      | [] -> cur = res
      | v :: tail -> aux tail (cur + v) || aux tail (cur * v) || aux tail (concat cur v))
  in
  aux (List.tl vals) (List.hd vals)
;;

let count_valid eqs =
  List.fold_left (fun acc eq -> if try_eq eq then acc + fst eq else acc) 0L eqs
;;

let main () =
  let eqs = parse "input.txt" in
  let vcount = count_valid eqs in
  Printf.printf "%Lu\n" vcount
;;

main ()
