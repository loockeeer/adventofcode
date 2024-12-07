let read filename =
  let ch = In_channel.open_text filename in
  let content = In_channel.input_all ch in
  In_channel.close ch;
  content;;

let count_par =
  String.fold_left (fun acc x -> if x = '(' then acc + 1 else acc - 1) 0;;

let basement_pos data =
  let n = String.length data in
  let rec aux i counter =
  if i = n then n
  else if counter < 0 then i
    else if data.[i] = '(' then aux (i+1) (counter+1)
  else aux (i+1) (counter-1)
  in aux 0 0;;
let main () =
  let data = read "input.txt" in
  Printf.printf "%d\n" (count_par data);
  Printf.printf "%d\n" (basement_pos data);;

main();;
