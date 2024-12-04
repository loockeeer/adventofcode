open Cmdliner
let pretty_number n =
  if n > 9 then "D" ^ (string_of_int n)
  else "D0" ^ (string_of_int n);;

(* Define the functions for each command *)
let do_setup ~config ~cookie ~path ~username ~github =
  Auto_setup.Config.save_config config {cookie;path;username;github};;

let file_exists name =
  try Unix.access name [Unix.F_OK]; true
  with
  | (Unix.Unix_error (Unix.ENOENT, _, _)) -> false
    | _ -> true;;

let format_boilerplate (conf:Auto_setup.Config.t) year =
  Printf.sprintf "(lang dune 3.16)

(name aoc_%d)

(generate_opam_files true)

(source
 (github %s))

(authors \"%s\")

(maintainers \"%s\")

(license LICENSE)

(package
 (name aoc_%d)
 (synopsis \"ocaml_of_aoc%d\")
 (description \"My aoc solutions for the year %d\")
 (depends ocaml dune)
 (tags
  (topics adventofcode %d)))" year conf.github conf.username conf.username year year year year;;

let create_year (conf:Auto_setup.Config.t) year =
  Unix.chdir (conf.path ^ "/");
  Sys.command ("mkdir -p "^(string_of_int year)) |> ignore;
  Unix.chdir (string_of_int year);
  if file_exists "dune-project" then ()
  else let ch = Out_channel.open_text "dune-project" in
  Out_channel.output_string ch (format_boilerplate conf year);
  Out_channel.close ch;;

let handle_error ~bad ~good code=
  if code <> 0 then
    Printf.printf "\o033[1m\o033[38;5;9m[error]\o033[0m\o033[38;5;8m %s\o033[0m\n" bad
  else
    Printf.printf "\o033[1m\o033[38;5;10m[done]\o033[0m\o033[38;5;8m  %s\o033[0m\n" good;;

let do_start ?year ?day config =
  let open Unix in
  let tm = localtime (time ()) in
  let current_year = 1900 + tm.tm_year in
  let current_day = tm.tm_mday in
  let year = (match year with Some y -> y | None -> current_year) in
  let day = (match day with Some d -> d | None -> current_day) in
  let config = Auto_setup.Config.read_config config in
  create_year config year;
  Unix.chdir (config.path ^ "/" ^ (string_of_int year));
  let pd = pretty_number day in
  ( if file_exists pd then ()
    else Sys.command (Printf.sprintf "dune init exe %s %s --pu %s > /dev/null 2>&1" pd pd pd) |> handle_error ~bad:"cannot create day executable" ~good:("day "^pd^" created"));
  Unix.chdir pd;
  Auto_setup.Input.download_input "input.txt" config.cookie year day |> handle_error ~bad:"cannot download input" ~good:"input downloaded";
  Printf.printf "\o033[1m\o033[38;5;1m> Happy Advent of Code !\o033[0m\n";;

let config_arg =
  let doc = "Config path." in
Arg.(value & opt (string) ("/home/lucas/.config/.aoc_as") & info ["c"; "config"] ~docv:"CONFIG" ~doc)

  (* Command-line argument converters *)
let cookie_arg =
  let doc = "Cookie string for setup." in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"COOKIE" ~doc)

let path_arg =
  let doc = "Advent of Code path." in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"PATH" ~doc)

let username_arg =
  let doc = "Username" in
  Arg.(required & pos 2 (some string) None & info [] ~docv:"USERNAME" ~doc)

let github_arg =
  let doc = "Github location" in
  Arg.(required & pos 3 (some string) None & info [] ~docv:"GITHUB" ~doc)

let year_arg =
  let doc = "Year (optional, defaults to the current year)." in
  Arg.(value & pos 0 (some int) None & info [] ~docv:"YEAR" ~doc)

let day_arg =
  let doc = "Day (optional, defaults to the current day)." in
  Arg.(value & pos 1  (some int) None & info [] ~docv:"DAY" ~doc)

(* Setup command *)
let setup_cmd =
  let doc = "Setup with a cookie string." in
  let term = Term.(const (fun config cookie path username github -> do_setup ~config ~cookie ~path ~username ~github ) $ config_arg $ cookie_arg $ path_arg $ username_arg $ github_arg) in
  Cmd.v (Cmd.info "setup" ~doc) term

(* Start command *)
let start_cmd =
  let doc = "Start with an optional year and day." in
  let term = Term.(const (fun config y d -> do_start ?year:y ?day:d config) $ config_arg $ year_arg $ day_arg) in
  Cmd.v (Cmd.info "start" ~doc) term

(* Main command *)
let main_cmd =
  let doc = "A CLI tool for setup and start operations." in
  Cmd.group (Cmd.info "file" ~doc) [setup_cmd; start_cmd]

(* Run the command-line interface *)
let () = exit (Cmd.eval main_cmd)
