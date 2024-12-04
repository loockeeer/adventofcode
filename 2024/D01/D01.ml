let readlines filename =
  let ic = open_in filename in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None
  in let rec loop acc = match try_read () with
    | Some s -> loop (s::acc)
    | None -> close_in ic; List.rev acc
  in loop [];;

let make_line_ok l =
  match l with
  | a::b::[] -> int_of_string a, int_of_string b
  | _ -> failwith "bad";;

let parse filename =
  let lines = readlines filename in
  List.map (fun l -> String.split_on_char ' ' l |> List.filter (fun x -> x <> "")) lines
  |> List.map (make_line_ok)
  |> List.split;;

let calculate_distance la lb =
  let sa = List.sort (fun x y -> x - y) la in
  let sb = List.sort (fun x y -> x - y) lb in
  List.combine sa sb
  |> List.map (fun (x,y) -> abs (x-y))
  |> List.fold_left (fun x y -> x + y) 0;;

let compute_similarity la lb =
  List.fold_left (fun acc va -> acc + (va * (List.length (List.find_all ((=)va) lb)))) 0 la;;

let main () =
  let la, lb = parse "input.txt" in
  Printf.printf "%d\n" (calculate_distance la lb);
  Printf.printf "%d\n" (compute_similarity la lb);;

main ();;
