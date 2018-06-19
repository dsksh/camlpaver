open Format
open Contractor
open Model_common
open Interval

let the_f t = fst t.lhs

let check_consistency t =
  let v = Expr.eval t.box (the_f t) in
  if is_strict_superset t.proj v then Proved
  else if t.proj = zero && width v = 0. then Proved
  else
    let v = intersect v t.proj in
    if is_empty v then NoSol
    else Unknown

let is_consistent is_lower t =
  let v0 = Box.get t.box t.vn in

  if is_empty v0 then false
  else
    let b = if is_lower then slice_lower v0 else slice_upper v0 in
    Box.set t.box t.vn b;
    let res = check_consistency t in

    (* restrore *)
    Box.set t.box t.vn v0;
    res <> NoSol

let shrink is_lower t =
  let v0 = Box.get t.box t.vn in
  let sf = 
    (* TO_CHECK *)
    if is_lower then Contractor_newton.sample_inf
    else Contractor_newton.sample_sup in

  let rec loop () =
    let v1 = Box.get t.box t.vn in
    let bnd = of_float (if is_lower then v0.sup else v0.inf) in
(*printf "\n";*)
    Contractor_newton.contract ~sample_fun:sf t.lhs t.vn t.box;

    if is_consistent is_lower t then
      (* restore the (other) bound *)
      Box.set t.box t.vn (join (Box.get t.box t.vn) bnd)

    else if is_empty (Box.get t.box t.vn) then
      Box.set t.box t.vn (join v1 bnd)

    else if v1 = Box.get t.box t.vn then
      (* TODO *)
      ()

    else begin
      (* cut off the other half of the interval *)
      let v = Box.get t.box t.vn in
      let mid = of_float (mid v) in
      let bnd = if is_lower then v.inf else v.sup in
      let bnd = of_float bnd in
      Box.set t.box t.vn (join mid bnd);
      loop ()
    end
  in 
  loop ()


(* the BC3 contraction procedure *)
let contract t =
  if check_consistency t = NoSol then begin
    Box.set t.box t.vn empty;
    NoSol
  end else begin

    (* shrink the lower bound *)
    if not (is_consistent true t) then
      shrink true t;

(*printf "after sl: %a@." Box.print t.box;*)

    if is_empty (Box.get t.box t.vn) then NoSol

    else begin 
      (* shrink the upper bound *)
      if not (is_consistent false t) then
        shrink false t;

(*printf "after su: %a@." Box.print t.box;*)

      check_consistency t
    end
  end

