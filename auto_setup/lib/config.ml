open Sexplib.Std

type t = {
  cookie: string;
  path: string;
  username: string;
  github: string;
} [@@deriving sexp];;

let save_config filename config =
  let oc = Out_channel.open_text filename in
  Out_channel.output_string oc (Sexplib0.Sexp.to_string (sexp_of_t config));
  Out_channel.close oc;;

let read_config filename =
  let ic = In_channel.open_text filename in
  In_channel.input_all ic
  |> Sexplib.Sexp.of_string
  |> t_of_sexp;;
