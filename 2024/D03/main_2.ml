let read_program filename =
  let ic = open_in filename in
  let try_read () =
    try Some (input_line ic) with
    | End_of_file -> None
  in
  let rec loop acc =
    match try_read () with
    | Some s -> loop (acc ^ s)
    | None ->
      close_in ic;
      acc
  in
  loop ""
;;

type reading_state_t =
  | Void
  | MulToken of char
  | DoToken of char
  | DontToken of char
  | NumberLeft of string
  | NumberRight of string * string

let mul_next_in_seq c = if c = 'm' then 'u' else if c = 'u' then 'l' else '('
let do_next_in_seq c = if c = 'd' then 'o' else if c = 'o' then '(' else ')'

let dont_next_in_seq c =
  if c = 'd'
  then 'o'
  else if c = 'o'
  then 'n'
  else if c = 'n'
  then '\''
  else if c = '\''
  then 't'
  else if c = 't'
  then '('
  else ')'
;;

let isdigit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let string_of_char = String.make 1

let compute_program prog =
  let n = String.length prog in
  let rec read i ops_acc rs dd =
    if i = n
    then List.fold_left (fun x y -> x + y) 0 ops_acc
    else (
      match rs with
      | Void ->
        if prog.[i] = 'm'
        then read i ops_acc (MulToken 'm') dd
        else if prog.[i] = 'd'
        then read i ops_acc (DoToken 'd') dd
        else read (i + 1) ops_acc Void dd
      | DoToken c ->
        if c = '(' && prog.[i] = 'n'
        then read i ops_acc (DontToken 'n') dd
        else if prog.[i] <> c
        then read i ops_acc Void dd
        else if prog.[i] = ')'
        then read (i + 1) ops_acc Void true
        else read (i + 1) ops_acc (DoToken (do_next_in_seq c)) dd
      | DontToken c ->
        if prog.[i] <> c
        then read i ops_acc Void dd
        else if prog.[i] = ')'
        then read (i + 1) ops_acc Void false
        else read (i + 1) ops_acc (DontToken (dont_next_in_seq c)) dd
      | MulToken c when c = '(' ->
        if prog.[i] = c
        then read (i + 1) ops_acc (NumberLeft "") dd
        else read i ops_acc Void dd
      | MulToken c ->
        if prog.[i] = c
        then read (i + 1) ops_acc (MulToken (mul_next_in_seq c)) dd
        else read i ops_acc Void dd
      | NumberLeft curr ->
        if prog.[i] = ','
        then read (i + 1) ops_acc (NumberRight (curr, "")) dd
        else if isdigit prog.[i]
        then read (i + 1) ops_acc (NumberLeft (curr ^ string_of_char prog.[i])) dd
        else read i ops_acc Void dd
      | NumberRight (before, curr) ->
        if prog.[i] = ')'
        then
          read
            (i + 1)
            (if dd
             then (int_of_string before * int_of_string curr) :: ops_acc
             else ops_acc)
            Void
            dd
        else if isdigit prog.[i]
        then
          read (i + 1) ops_acc (NumberRight (before, curr ^ string_of_char prog.[i])) dd
        else read i ops_acc Void dd)
  in
  read 0 [] Void true
;;

let main () =
  let program = read_program "input.txt" in
  let output = compute_program program in
  Printf.printf "%d\n" output
;;

main ()
