open Format
open Lexing
open Pretty

let usage = "usage: main.exe [options] input.bch"

let spec = [
  "-e", Arg.Float (fun e -> Solver.eps := e), 
  " Set the precision of paving";
  "-n", Arg.Int (fun n -> Solver.max_n := n), 
  " Set the max number of solver iteration";
  "-bfs", Arg.Set Solver.bfs,
  " Use the breadth-first search";
  "-dfs", Arg.Clear Solver.bfs,
  " Use the depth-first search";
  "-g", Arg.Set Util.debug, 
  " Set the debug flag";
]

let file = ref "stdin"
let cin =
  let ofile = ref None in
  Arg.parse spec (fun s -> ofile := Some s) usage; 
  match !ofile with
  | Some f -> file := f ; open_in f
  | None -> stdin

let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  printf "File \"%s\", line %d, characters %d-%d: " !file l fc lc

let () =
  let lb = from_channel cin in
  try
    (* parse *)
    let vt,iv_l,cs = Parser.main Lexer.token lb in 
    close_in cin;

    (* create the scope *)
    let sc = Box.Scope.make vt iv_l in
    let box = Box.make sc in
    if !Util.debug then printf "%a@.@." Box.print box;

    if !Util.debug then begin
      let pr c = printf "%a;@.@." print_ptree c in
      let _ = List.map pr cs in () 
    end;

    let vs = sc.vn_list in
    let cs = List.map (Expr.mk_constr vs) cs in
    if !Util.debug then begin
      let pr c = printf "%a;@.@." print_constr c in
      let _ = List.map pr cs in ()
    end;

    (* solve *)
    let ef = 
      if !Solver.bfs then Solver.extract_queue
      else Solver.extract_stack in
    let iff = 
      if !Solver.max_n >= 0 then Solver.is_nloops_exceed
      else Solver.is_empty in
    let sols = Solver.solve ~extract:ef ~is_finished:iff cs box in

    (* print the result *)
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
  | Lexer.Lexical_error s ->
      report (lexeme_start_p lb, lexeme_end_p lb);
      printf "lexical error (%s)@;@." s;
      exit 1
  | Parsing.Parse_error ->
      let  loc = (lexeme_start_p lb, lexeme_end_p lb) in
      report loc;
      printf "syntax error@;@.";
      exit 1  
  | Util.Error e ->
      printf "error: %a\n@." Util.report e;  
      exit 1
  | _ ->
      printf "Unexpected error@;@.";
      exit 1

