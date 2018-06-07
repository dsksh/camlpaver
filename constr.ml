open Hashcons
open Expr
open Model_common
open Ptree
open Util

type t = node hash_consed
and node =
  | C of rel_op * Expr.dual * Expr.dual
  | G of t * t (* guarded constraint *)
  | P of t * t (* list constructor *)
  | True

module Constr_node = struct
  type t = node

  let equal c1 c2 = 
    match c1, c2 with
    | C (op1,e1,f1), C (op2,e2,f2) -> op1 == op2 && e1 == e2 && f1 == f2
    | G (c1,d1), G (c2,d2) -> c1 == c2 && d1 == d2
    | P (c1,d1), P (c2,d2) -> c1 == c2 && d1 == d2
    | True, True -> true
    | _ -> false

  (* TODO *)
  let hash = function
    | C (op,(e1,_),(e2,_)) -> abs (19*(19*e1.hkey + e2.hkey) +
        (match op with | Oeq -> 1 | One -> 2 | Olt -> 3 | Ole -> 4 | Ogt -> 5 | Oge -> 6)
        + 2 )
    | G (c1,c2) -> abs (23*c1.hkey + c2.hkey + 1)
    | P (c1,c2) -> abs (19*c1.hkey + c2.hkey + 1)
    | True -> Hashtbl.hash 0
end

module Hconstr = Make(Constr_node)

let ht = Hconstr.create 61

let true_ = Hconstr.hashcons ht True

let rec make vs = function
  | _, Prel (op,e1,e2) -> 
      let e1 = mk_dual_expr vs e1 in
      let e2 = mk_dual_expr vs e2 in
      Hconstr.hashcons ht (C (op,e1,e2))
  | _, Pif (c1,c2) ->
      let c1 = List.map (fun c -> make vs c) c1 in
      let c2 = List.map (fun c -> make vs c) c2 in
      let c1 = if List.length c1 = 1 then List.hd c1 else mk_list c1 in
      let c2 = if List.length c2 = 1 then List.hd c2 else mk_list c2 in
      Hconstr.hashcons ht (G (c1,c2))
  | _, PifElse _ -> 
      error (Unsupported "if-then-else")

and mk_list cs = 
  let cmp c1 c2 = compare c1.tag c2.tag in
  let cs = List.sort cmp cs in
  let f c = function
    | None -> Some c
    | Some c' -> Some (Hconstr.hashcons ht (P (c,c'))) in
  match List.fold_right f cs None with
  | None -> true_
  | Some cs -> cs

let rec to_list c = 
  match c.node with
  | P (c1,c2) -> List.append (to_list c1) (to_list c2)
  | _ -> [c]
