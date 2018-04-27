open Hashcons
open Expr
open Model_common

type t = node hash_consed
and node =
  | C of rel_op * Expr.dual * Expr.dual
  | G of t * t
  | L of t list

module Constr_node = struct
  type t = node

  let rec equal c1 c2 = 
    match c1, c2 with
    | C (op1,e1,f1), C (op2,e2,f2) -> op1 == op2 && e1 == e2 && f1 == f2
    | G (c1,d1), G (c2,d2) -> c1 == c2 && d1 == d2
    | L [], L[] -> true
    | L (c1::r1), L (c2::r2) -> c1 == c2 && equal (L r1) (L r2)
    | L _, L _ -> false
    | c1, L [c2] -> c1 == c2.node
    | L [c1], c2 -> c1.node == c2
    | _ -> false

  (* TODO *)
  let hash = function
    | C (op,(e1,_),(e2,_)) -> abs (19*(19*e1.hkey + e2.hkey) +
        (match op with | Oeq -> 1 | Olt -> 2 | Ole -> 3 | Ogt -> 4 | Oge -> 5)
        + 2 )
    | G (c1,c2) -> abs (19*c1.hkey + c2.hkey + 1)
    | L cs -> 
        let f h c = 19*h + c.hkey in
        abs ((List.fold_left f (Hashtbl.hash 0) cs) + (List.length cs) - 1)
end

module Hconstr = Make(Constr_node)

let ht = Hconstr.create 61

let make vs = function
  | _, (op,e1,e2) -> 
      let e1 = mk_dual_expr vs e1 in
      let e2 = mk_dual_expr vs e2 in
      Hconstr.hashcons ht (C (op,e1,e2))

let mk_list cs = 
  let cmp c1 c2 = -(compare c1.tag c2.tag) in
  let cs = List.sort cmp cs in
  Hconstr.hashcons ht (L cs)

