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
    let map,dom,cs = Parser.main Lexer.token lb in 
    close_in cin;

    let sc = Box.Scope.create map dom in
    printf "%a@.@." Box.Scope.print sc;

    let pr c = printf "%a;@.@." print_ptree c in
    let _ = List.map pr cs in

    let vs = Box.Scope.to_var_list sc in
    let cs = List.map (Expr.mk_constr vs) cs in
    let pr c = printf "%a;@.@." print_constr c in
    let _ = List.map pr cs in

    ()
  with
  | _ ->
    printf "unexpected error\n@.";
    exit 1

