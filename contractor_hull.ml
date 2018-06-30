open Format
open Array
open Contractor
open Hashcons
open Expr
open Model_common
open Interval
open Util

exception Empty_result

type attr = { fwd : Interval.t; mutable bwd : Interval.t; }

let get_attr at i = match at.(i) with Some a -> a | None -> error Unexpected

let set_bwd at expr v = 
  let a = get_attr at (expr.tag) in
  let v = intersect v a.bwd in
  if not (Interval.is_empty v) then 
    a.bwd <- v
  else
    raise Empty_result


let rec fwd_eval at box expr = 
  match at.(expr.tag) with 
  | Some { fwd=v; _ } -> v (* TODO *)

  | None ->
    let rc e = fwd_eval at box e in
  
    let v = match expr.node with
    | Var n -> 
        Box.get box n
  
    | Val v -> v
  
    | App1 (op,e) ->
        let v = rc e in
        (impl_of_op1 op) v
  
    | Pow (n,e) ->
        let v = rc e in
        Interval.pow v n
  
    | App2 (op,e1,e2) ->
        let v1 = rc e1 in
        let v2 = rc e2 in
        let v = (impl_of_op2 op) v1 v2
        in
(*printf "  App2 %s: %a\n" (str_of_op2 op) Interval.print v;*)
        v
    in 
    let a = { fwd=v; bwd=v; } in
    at.(expr.tag) <- Some a;
    v

let rec bwd_propag at expr box =
  let rc e = bwd_propag at e box in

  let fwd e = (get_attr at (e.tag)).fwd in
  let bwd e = (get_attr at (e.tag)).bwd in
  let set e v = set_bwd at e v in

  match expr.node with
  | Var n -> 
      let v = intersect (Box.get box n) (bwd expr) in
      Box.set box n v

  | Val v -> ()

  | App1 (Osqrt,e) ->
      let bwd = bwd expr in
      if is_empty bwd || bwd.sup < 0. then
        set e empty
      else if bwd.inf < 0. then
        let i = { inf=0.; sup=bwd.sup } in
        set e (i *$ i)
      else 
        set e (bwd *$ bwd);
      rc e

  | App1 (op,_) -> error (Unsupported (Model_common.str_of_op1 op))

  | Pow (n,e) ->
      let p = root (bwd expr) n in
      if n mod 2 = 0 then
        let pp = intersect p (fwd e) in
        let np = intersect (~-$ p) (fwd e) in
        set e (Interval.join pp np)
      else
        set e p;
      rc e

  | App2 (Oadd,e1,e2) ->
      set e1 ((bwd expr) -$ (fwd e2));
(*printf "bwd add l: %a = %a - %a\n" Interval.print (bwd e1) Interval.print (bwd expr) Interval.print (fwd e2);*)
      rc e1;
      set e2 ((bwd expr) -$ (bwd e1));
(*printf "bwd add r: %a = %a - %a\n" Interval.print (bwd e2) Interval.print (bwd expr) Interval.print (fwd e1);*)
      rc e2

  | App2 (Osub,e1,e2) ->
      set e1 ((bwd expr) +$ (fwd e2));
      rc e1;
      set e2 ((bwd e1) -$ (bwd expr));
      rc e2

  | App2 (Omul,e1,e2) ->
      if not (is_superset (fwd e2) zero) then begin
        set e1 ((bwd expr) /$ (fwd e2));
(*printf "bwd mul l: %a = %a / %a\n" Interval.print (bwd e1) Interval.print (bwd expr) Interval.print (fwd e2);*)
      end;
      rc e1;
      if not (is_superset (bwd e1) zero) then begin
        set e2 ((bwd expr) /$ (bwd e1));
(*printf "bwd mul r: %a = %a / %a\n" Interval.print (bwd e2) Interval.print (bwd expr) Interval.print (fwd e1);*)
      end;
      rc e2

  | App2 (Odiv,e1,e2) ->
      set e1 ((bwd expr) *$ (fwd e2));
      rc e1;
      if not (is_superset (bwd expr) zero) then
        set e2 ((bwd e1) /$ (bwd expr));
      rc e2


let contract t =
  (*let op, (e1,_), (e2,_) = constr in*)

  let lhs = fst t.lhs in

  let at = Array.make (get_max_tag ()) None in

  (* forward propagation *)
  let v = fwd_eval at t.box lhs in

(*printf "after fwd:@.";
printf "%a vs. %a@." Interval.print v Interval.print t.proj; *)

  if is_superset t.proj v && (t.bnd || not (is_contained v 0.)) then Proved
  else begin

    (* backward propagation *)
    try
      let v = Interval.intersect t.proj v in
      if not (Interval.is_empty v) &&
         (t.bnd || v <> Interval.zero) then 
      begin
        set_bwd at lhs v;
        bwd_propag at lhs t.box
      end else
        raise Empty_result;

(*printf "after bwd:@.";
printf "%a@." Box.print t.box; *)

    Unknown

  with Empty_result -> 
    t.box.v.(0) <- Interval.empty;
    NoSol

  end
