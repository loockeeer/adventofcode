let row, col = 2978, 3083

let get_code row col =
  let rec aux i j c =
    if i = row && j = col
    then c
    else if i < 1
    then aux j 1 c
    else aux (i - 1) (j + 1) (c * 252533 mod 33554393)
  in
  aux 1 1 20151125
;;

let main () = Printf.printf "%d\n" (get_code row col);;

main ()
