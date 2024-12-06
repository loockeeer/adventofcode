let readfile filename =
  let ch = In_channel.open_text filename in
  let data = In_channel.input_lines ch in
  In_channel.close ch;
  data
;;

type direction_t =
  | North
  | South
  | East
  | West

type cell_t =
  | Void
  | Obs

type pos_t =
  { x : int
  ; y : int
  }

let grow_direction { x; y } dir =
  match dir with
  | North -> { x; y = y - 1 }
  | South -> { x; y = y + 1 }
  | East -> { x = x + 1; y }
  | West -> { x = x - 1; y }
;;

let next_dir = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North
;;

let check_bounds width height { x; y } = 0 <= x && x < width && 0 <= y && y < height

type inst_t =
  { width : int
  ; height : int
  ; mat : cell_t array array
  ; spos : pos_t
  }

let pos_match (n, e, s, w) dir =
  match dir with
  | North -> n
  | East -> e
  | South -> s
  | West -> w
;;

let pos_add (n, e, s, w) dir =
  match dir with
  | North -> true, e, s, w
  | East -> n, true, s, w
  | South -> n, e, true, w
  | West -> n, e, s, true
;;

let parse filename =
  let lines = readfile filename in
  let spost = ref { x = 0; y = 0 } in
  let mat =
    List.mapi
      (fun y line ->
        String.to_seq line
        |> Array.of_seq
        |> Array.mapi (fun x c ->
          if c = '^' then spost := { x; y } else ();
          if c = '.' then Void else if c = '#' then Obs else Void))
      lines
    |> Array.of_list
  in
  { width = Array.length mat.(0); height = Array.length mat; mat; spos = !spost }
;;

let calculate_path inst =
  let cpos = ref inst.spos in
  let reached = Array.make_matrix inst.height inst.width 0 in
  let current_dir = ref North in
  while check_bounds inst.width inst.height !cpos do
    let new_pos = grow_direction !cpos !current_dir in
    if check_bounds inst.width inst.height new_pos
    then (
      match inst.mat.(new_pos.y).(new_pos.x) with
      | Void ->
        cpos := new_pos;
        reached.(new_pos.y).(new_pos.x) <- 1
      | Obs -> current_dir := next_dir !current_dir)
    else cpos := new_pos
  done;
  reached.(inst.spos.y).(inst.spos.x) <- 1;
  Array.fold_left (fun acc x -> acc + Array.fold_left ( + ) 0 x) 0 reached
;;

exception Leaveloop

let check_loop inst obspos =
  let cpos = ref inst.spos in
  inst.mat.(obspos.y).(obspos.x) <- Obs;
  let reached = Array.make_matrix inst.height inst.width (false, false, false, false) in
  let current_dir = ref North in
  try
    while check_bounds inst.width inst.height !cpos do
      let new_pos = grow_direction !cpos !current_dir in
      if pos_match reached.(!cpos.y).(!cpos.x) !current_dir
      then raise Leaveloop
      else if check_bounds inst.width inst.height new_pos
      then (
        match inst.mat.(new_pos.y).(new_pos.x) with
        | Void ->
          reached.(!cpos.y).(!cpos.x) <- pos_add reached.(!cpos.y).(!cpos.x) !current_dir;
          cpos := new_pos
        | Obs -> current_dir := next_dir !current_dir)
      else cpos := new_pos
    done;
    inst.mat.(obspos.y).(obspos.x) <- Void;
    false
  with
  | Leaveloop ->
    inst.mat.(obspos.y).(obspos.x) <- Void;
    true
;;

let search_loops inst =
  let count = ref 0 in
  for x = 0 to inst.width - 1 do
    for y = 0 to inst.height - 1 do
      if inst.mat.(y).(x) = Void
         && (not (inst.spos.x = x && inst.spos.y = y))
         && check_loop inst { x; y }
      then count := !count + 1
      else ()
    done
  done;
  !count
;;

let main () =
  let inst = parse "input.txt" in
  let count = calculate_path inst in
  let found = search_loops inst in
  Printf.printf "%d\n" count;
  Printf.printf "%d\n" found
;;

main ()
