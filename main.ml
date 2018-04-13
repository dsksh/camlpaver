open Format
open Lexing
open Pretty
open Model_common

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
    let dom,cs = Parser.main Lexer.token lb in 
    let pr k d = printf "%s in [%f,%f];@." k (fst d) (snd d) in
    MDom.iter pr dom;
    let pr c = printf "%a;@." print_constr c in
    let _ = List.map pr cs in
    let _ = Interval.intv_add 0. 1. 2. 3. in
    close_in cin;
  with
  | _ ->
    printf "unexpected error\n@.";
    exit 1
