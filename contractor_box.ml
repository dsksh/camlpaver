
open Model_common
open Interval

type t = {
  fn : Expr.expr * Expr.expr list;
  vn : string;
  box : Box.t;
  proj : Interval.t;
}

let init constr vn box =
  let op,e1,e2 = constr in
  let fn = Expr.mk_diff_expr (Box.get_vn_list box) e1 e2 in
  let proj = match op with
  | Oeq -> zero
  | Olt | Ole -> negative
  | Ogt | Oge -> positive
  in
  { fn=fn; vn=vn; box=box; proj=proj; }

let the_f t = fst t.fn

type r = NoSol | Proved | Unknown

let check_consistency t =
  let v = intersect (Expr.eval t.box (the_f t)) t.proj in
  if is_empty v then NoSol
  else if width v = 0. then Proved
  else if is_superset v zero then Proved
  else Unknown

let is_consistent is_lower t =
  let v0 = Box.get t.box t.vn in

  let b = if is_lower then slice_lower v0 else slice_upper v0 in
  Box.set t.box t.vn b;
  let res =  check_consistency t in

  (* restrore *)
  Box.set t.box t.vn v0;
  res <> NoSol

let shrink is_lower t =
  let v0 = Box.get t.box t.vn in
  let sf = if is_lower then Contractor_newton.sample_inf 
    else Contractor_newton.sample_sup in
  let rec loop () =
    Contractor_newton.contract ~sample_fun:sf t.fn t.vn t.box;
    if is_consistent is_lower t || is_empty (Box.get t.box t.vn) then
      let bnd = of_float (if is_lower then v0.sup else v0.inf) in
      Box.set t.box t.vn (join (Box.get t.box t.vn) bnd)
    else begin
      let v = Box.get t.box t.vn in
      let bnd = if is_lower then v.inf else v.sup in
      Box.set t.box t.vn (Interval.make (mid v) bnd);
      loop ()
    end
  in loop ()


(* the BC3 contraction procedure *)
let contract t =
  if check_consistency t = NoSol then begin
    Box.set t.box t.vn empty;
    NoSol
  end else begin

    (* shrink the lower bound *)
    if not (is_consistent true t) then
      shrink true t;

    Format.printf "after sl:\n";
    Format.printf "%a\n" Box.print t.box;

    if is_empty (Box.get t.box t.vn) then NoSol

    else begin 
      (*let lb = (Box.get t.box t.vn).inf in*)
  
      (* shrink the upper bound *)
      if not (is_consistent false t) then
        shrink false t;

      Format.printf "after su:\n";
      Format.printf "%a\n" Box.print t.box;

      check_consistency t
    end
  end

