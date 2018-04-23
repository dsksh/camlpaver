open Format
open Hashcons
open Expr
open Model_common
open Interval

type attr = { fwd : Interval.t; mutable bwd : Interval.t; }

let set_bwd at expr v = 
  let a = Hashtbl.find at expr.tag in
  a.bwd <- v


let rec fwd_eval at box expr = 
  try let a = Hashtbl.find at expr.tag in a.fwd
  with Not_found ->
    let rc e = fwd_eval at box e in
  
    let v = match expr.node with
    | Var n -> 
(*printf "  Var %s: %a\n" n Interval.print box.v.(Hashtbl.find box.s.table n);*)
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
    let a = { fwd=v; bwd=Interval.zero; } in
    Hashtbl.add at expr.tag a; 
    v


let rec bwd_propag at expr box =
  let rc e = bwd_propag at e box in

  let fwd e = (Hashtbl.find at e.tag).fwd in
  let bwd e = (Hashtbl.find at e.tag).bwd in
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

  | Pow (n,e) ->
      if n mod 2 = 0 then
        let p = root (bwd expr) n in
        let pp = intersect p (fwd e) in
        let np = intersect (~-$ p) (fwd e) in
        if is_empty pp || is_empty np then
          set e empty
        else
          set e (join pp np);
      rc e

  | App2 (Oadd,e1,e2) ->
      set e1 ((bwd expr) -$ (fwd e2));
      rc e1;
      set e2 ((bwd expr) -$ (fwd e1));
      rc e2

  | App2 (Osub,e1,e2) ->
      set e1 ((bwd expr) +$ (fwd e2));
      rc e1;
      set e2 ((bwd expr) -$ (fwd e1));
      rc e2

  | App2 (Omul,e1,e2) ->
      set e1 ((bwd expr) /$ (fwd e2));
      rc e1;
      set e2 ((bwd expr) /$ (fwd e1));
      rc e2

  | App2 (Odiv,e1,e2) ->
      set e1 ((bwd expr) *$ (fwd e2));
      rc e1;
      set e2 ((bwd expr) /$ (fwd e1));
      rc e2

  | _ -> assert false (* TODO *)


let contract constr box =
  let at = Hashtbl.create 61 in
  let op, (e1,_), (e2,_) = constr in

  (* forward propagation *)
  let v1 = fwd_eval at box e1 in
  let v2 = fwd_eval at box e2 in

  printf "after fwd:@.";
  printf "%a@." Interval.print v1;
  printf "%a@." Interval.print v2;

  (* backward propagation *)
  begin match op with
  | Oeq ->
      let v = Interval.intersect v1 v2 in
      set_bwd at e1 v;
      bwd_propag at e1 box;
      set_bwd at e2 v;
      bwd_propag at e2 box
  | Olt
  | Ole ->
      let v = join (of_float neg_infinity) v2 in
      set_bwd at e1 (intersect v1 v);
      bwd_propag at e1 box;
      let v = join v1 (of_float infinity) in
      set_bwd at e2 (intersect v2 v);
      bwd_propag at e2 box
  | Ogt
  | Oge ->
      let v = join v2 (of_float infinity) in
      set_bwd at e1 (intersect v1 v);
      bwd_propag at e1 box;
      let v = join (of_float infinity) v1 in
      set_bwd at e2 (intersect v2 v);
      bwd_propag at e2 box;
  end;

  printf "after bwd:@.";
  printf "%a@." Box.print box

