let readlines filename =
  let ic = open_in filename in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None
  in let rec loop acc = match try_read () with
    | Some s -> loop (s::acc)
    | None -> close_in ic; List.rev acc
  in loop [];;

let parse filename =
  let lines = readlines filename in
  List.map (fun l -> String.split_on_char ' ' l |> List.map (int_of_string)) lines;;

let rec check_report rep prev =
  match rep with
  | [] -> true
  | _::[] -> true
  | a::b::t ->
  let x = abs (a-b) in
  let bl = (1<=x && x<= 3) in
  if prev = 0 then bl && (check_report (b::t) (if a < b then -1 else 1))
  else if prev = 1 then (
    if a < b then false
    else bl && (check_report (b::t) prev)
    )
  else (
        if a > b then false
        else bl && (check_report (b::t) prev)
    );;

let check_reports = List.fold_left (fun x y -> x + (if (check_report y 0 = true) then 1 else 0)) 0;;

let rec remove_ith l j i =
  match l with
  | [] -> []
  | _::t when i = j -> t
  | x::t -> x::(remove_ith t (j+1) i);;

let check_report_dampened report =
  let rec check rep sign =
  match rep with
  | [] | _::[] -> true
  | a::b::t -> (
    let diff = abs (a-b) in
    let comp = 1 <= diff && diff <= 3 in
    let newsign = (if sign = 0 then (if a < b then -1 else 1) else sign) in
    if newsign = 1 then (a>b) && comp && (check (b::t) newsign)
    else (a<b) && comp && (check (b::t) newsign)
  )
  in let n = List.length report
  in let rec dampening_loop i =
    if i = n then false
    else (check (remove_ith report 0 i) 0) || (dampening_loop (i+1))
  in check report 0 || dampening_loop 0;;

let check_reports_dampened = List.fold_left (fun x y -> x + (if (check_report_dampened y = true) then 1 else 0)) 0;;

let main () =
  let reports = parse "input.txt" in
  Printf.printf "%d\n" (check_reports reports);
  Printf.printf "%d\n" (check_reports_dampened reports);;

main ();;
