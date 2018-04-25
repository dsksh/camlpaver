open Format
open Lexing
open Pretty

let usage = "usage: main.exe [options] input.bch"

let spec = [
  "-e",  Arg.Float (fun e -> Solver.eps := e), 
  " Set the precision of paving";
]

let file = ref "stdin"
let cin =
  let ofile = ref None in
  Arg.parse spec (fun s -> ofile := Some s) usage; 
  match !ofile with
  | Some f -> file := f ; open_in f
  | None -> stdin

let () =
  let lb = from_channel cin in
  try
    let vt,iv_l,cs = Parser.main Lexer.token lb in 
    close_in cin;

    (* create the scope *)
    let sc = Box.Scope.make vt iv_l in
    let box = Box.make sc in
    printf "%a@.@." Box.print box;

    let pr c = printf "%a;@.@." print_ptree c in
    let _ = List.map pr cs in

    let vs = sc.vn_list in
    let cs = List.map (Expr.mk_constr vs) cs in
    let pr c = printf "%a;@.@." print_constr c in
    let _ = List.map pr cs in

    let sols = Solver.solve cs box in
    if List.length sols = 0 then printf "@.no solution@."
    else
      let f = ref true in
      let pr sol = 
        if !f then f := false else printf ",@]@;@.";
        printf "@[<2>%a" Pretty_mathematica.print_box sol in
      printf "{@.";
      let _ = List.map pr sols in ();
      printf "@.}@.";

    ()
  with
  | _ ->
    printf "unexpected error\n@.";
    exit 1

