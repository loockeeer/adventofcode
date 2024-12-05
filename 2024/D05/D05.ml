let readfile filename =
  let ch = In_channel.open_text filename in
  let rec loop acc_ordering acc_updates trigger =
    match In_channel.input_line ch with
    | None -> List.rev acc_ordering, List.rev acc_updates
    | Some line when line = "" -> loop acc_ordering acc_updates true
    | Some line when trigger = false ->
      loop
        ((match String.split_on_char '|' line with
          | [ a; b ] -> int_of_string a, int_of_string b
          | _ -> failwith "tg")
         :: acc_ordering)
        acc_updates
        trigger
    | Some line ->
      loop
        acc_ordering
        ((String.split_on_char ',' line |> List.map int_of_string) :: acc_updates)
        trigger
  in
  loop [] [] false
;;

let build_order_rule order =
  let mat = Array.make_matrix 100 100 true in
  List.iter (fun (x, y) -> mat.(y).(x) <- false) order;
  let out x y = mat.(x).(y) in
  out
;;

let check_in_order order update =
  let ( $> ) = build_order_rule order in
  let rec aux cl =
    match cl with
    | [] -> true
    | x :: tail -> List.for_all (( $> ) x) tail && aux tail
  in
  aux update
;;

let get_valid order =
  let check = check_in_order order in
  List.filter check
;;

let get_middles order updates =
  get_valid order updates
  |> List.map (fun l -> List.nth l (List.length l / 2))
  |> List.fold_left ( + ) 0
;;

let sort_updates order =
  let rel = build_order_rule order in
  List.map (List.sort (fun x y -> if rel x y then -1 else 1))
;;

let main () =
  let order, updates = readfile "input.txt" in
  let middle_sum = get_middles order updates in
  let vcount = List.length (sort_updates order updates |> get_valid order) in
  let newcount = get_middles order (sort_updates order updates) - middle_sum in
  Printf.printf "%d\n" middle_sum;
  Printf.printf "%d / %d\n" vcount (List.length updates);
  Printf.printf "%d\n" newcount
;;

main ()
