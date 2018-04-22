open Format
open Lexing
open Pretty
open Model_common
open Interval

(* TODO *)
let spec = [
]

let file = ref "stdin"
let cin =
  let ofile = ref None in
  Arg.parse spec (fun s -> ofile := Some s) "usage: ..."; (* TODO *)
  match !ofile with
    | Some f -> file := f ; open_in f
    | None -> stdin

let () =
  let lb = from_channel cin in
  try
    let vt,iv_l,cs = Parser.main Lexer.token lb in 
    close_in cin;

    let sc = Box.Scope.make vt iv_l in
    let box = Box.make sc in
    printf "%a@.@." Box.print box;

    let pr c = printf "%a;@.@." print_ptree c in
    let _ = List.map pr cs in

    let vs = sc.vn_list in
    let cs = List.map (Expr.mk_constr vs) cs in
    let pr c = printf "%a;@.@." print_constr c in
    let _ = List.map pr cs in

    let ctr c = Contractor_hull.contract c box in
    let _ = List.map ctr cs in

    ()
  with
  | _ ->
    printf "unexpected error\n@.";
    exit 1

