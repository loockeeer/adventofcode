let array_of_string s =
  String.to_seq s |> Array.of_seq;;
type data_t = {
  arr: char array array;
  width: int;
  height: int;
}
let read_file filename =
  let ch = In_channel.open_text filename in
  let out = In_channel.input_lines ch
  |> Array.of_list
  |> Array.map array_of_string in
  In_channel.close ch;
  {arr=out; width=Array.length (out.(0));height=Array.length out};;

let next_in_seq c =
  match c with
  | `Void -> `Char 'X'
  | `Char c -> (
    if c = 'X' then `Char 'M'
    else if c = 'M' then `Char 'A'
    else if c = 'A' then `Char 'S'
    else if c = 'S' then `Done
    else `Void)
  | `Done -> `Done;;
type pos_t = {
  x: int;
  y: int;
};;
let check_bounds width height {x;y} =
  0 <= x && x < width && 0 <= y && y < height;;
type direction = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest;;
let dirs = [North;NorthEast;East;SouthEast;South;SouthWest;West;NorthWest];;
let grow_direction dir {x;y} =
  match dir with
  | North -> {x=x;y=y-1}
  | NorthEast -> {x=x+1;y=y-1}
  | East -> {x=x+1;y}
  | SouthEast -> {x=x+1;y=y+1}
  | South -> {x;y=y+1}
  | SouthWest -> {x=x-1;y=y+1}
  | West -> {x=x-1;y}
  | NorthWest -> {x=x-1;y=y-1};;



let match_dir dt dir {x;y} =
  let rec aux cp rs =
      match rs with
      | `Void -> false
      | `Done -> true
      | `Char c -> (check_bounds dt.width dt.height cp) && (dt.arr.(cp.y).(cp.x) = c) && (aux (grow_direction dir cp) (next_in_seq rs))
  in aux {x;y} (next_in_seq `Void);;

let find_occurences dt =
  let count = ref 0 in
  for x = 0 to (dt.width -1) do
    for y = 0 to (dt.height -1) do
        count := !count + List.fold_left (fun acc d -> if match_dir dt d {x;y} then acc+1 else acc) 0 dirs;
    done;
  done;
  !count;;

let count_mas dt =
  let count = ref 0 in
  for x = 1 to (dt.width -2) do
    for y = 1 to (dt.height -2) do
      let check d c = let np = grow_direction d {x;y} in (=) dt.arr.(np.y).(np.x) c in
      if dt.arr.(y).(x)= 'A' &&
        ((check NorthWest 'M' && check SouthEast 'S') || (check NorthWest 'S' && check SouthEast 'M')) &&
        ((check NorthEast 'M' && check SouthWest 'S') || (check NorthEast 'S' && check SouthWest 'M')) then
        count := !count +1
      else ()
    done
  done;
  !count;;
let main () =
  let dt = read_file "input.txt" in
  Printf.printf "%d\n%d\n" (find_occurences dt) (count_mas dt);;

main();;
