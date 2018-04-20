open Format
open Lexing
open Pretty
open Model_common
open Interval

let spec = [
]

let file = ref "stdin"
let cin =
  let ofile = ref None in
  Arg.parse spec (fun s -> ofile := Some s) "usage: ...";
  match !ofile with
    | Some f -> file := f ; open_in f
    | None -> stdin

let () =
  let lb = from_channel cin in
  try
    let sc,dom,cs = Parser.main Lexer.token lb in 
    close_in cin;

    let pr n i = printf "%s in %a;@." n Interval.print (List.nth dom i) in
    Hashtbl.iter pr sc;
    let pr c = printf "%a;@." print_constr c in
    let _ = List.map pr cs in

    ()
  with
  | _ ->
    printf "unexpected error\n@.";
    exit 1
