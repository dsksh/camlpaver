open Constr
open Hashcons
open Contractor

type t = { mutable last : int; mutable sp : int option; }

let bfs = ref true
let eps = ref 1e-2
let max_n = ref (-1)

let append node dq = 
  Deque.cons node dq

let extract_stack dq = 
  let node = Deque.first dq in
  let dq = Deque.tail dq in
  node, dq

let extract_queue dq = 
  let node = Deque.last dq in
  let dq = Deque.eject dq in
  node, dq

let rec select_bb box ctx =
  if Box.is_empty box then None
  else begin
    ctx.last <- 
      if ctx.last < (Box.dim box)-1 then succ ctx.last 
      else 0;
  
    if ctx.sp = Some ctx.last then
      (ctx.sp <- None; None)
    else begin
      if ctx.sp = None then 
        ctx.sp <- Some ctx.last;
  
      let vn = Box.get_vn box ctx.last in
      if Interval.width (Box.get box vn) < !eps then
        select_bb box ctx
      else
        (ctx.sp <- None; Some vn)
    end
  end

let rec contract c box =
(*Format.printf "contract: %a@." Pretty.print_constr c;*)
  if Box.is_empty box then NoSol
  else
    match c.node with
    | C (op,e1,e2) -> 
        let t = Contractor.init (op,e1,e2) box in
        apply_contractors t

    | G (ac,tc) ->
        if check_entailment box ac then
          contract tc box
        else
          Unknown

    | P (c1,c2) -> 
        let r1 = contract c1 box in
        if r1 <> NoSol then

          let r2 = contract c2 box in

          (* merge the results *)
          if r2 <> NoSol then
            if r1 = Proved && r2 = Proved then Proved
            else Unknown
          else NoSol

        else NoSol

    | True -> Proved

and check_entailment box c =
  match c.node with
  | C (op,e1,e2) ->
      let b = Box.copy box in
      (* negation of the ask constraint *)
      let t = Contractor.init (Model_common.negate_rop op,e1,e2) b in
      let r = apply_contractors t in
      (* empty result implies the entailment *)
      r = NoSol

  | P (c1,c2) -> 
      (* all the ask constraints should be entailed *)
      if check_entailment box c1 then 
        check_entailment box c2
      else
        false

  | _ -> assert false

and apply_contractors t =
  (* apply HC4 *)
  let r = Contractor_hull.contract t in

  (* apply BC3 *)
  let ctr r vn =
    if r == NoSol || r == Proved then r
    else (* Unknown *) begin 
      Contractor.set_var t vn;
      Contractor_box.contract t
    end in
  let r = List.fold_left ctr r (Box.get_vn_list t.box) in

  r

let split vn box =
  let v0 = Box.get box vn in
  let b1 = box in
  let b2 = Box.copy box in
  let m = Interval.mid v0 in
  Box.set b1 vn (Interval.make v0.inf m);
  Box.set b2 vn (Interval.make m v0.sup);
  b1, b2

let clone_ctx ctx = { last=ctx.last; sp=ctx.sp; }

let is_empty _n dq = Deque.is_empty dq
let is_nloops_exceed n dq = is_empty n dq || n > !max_n

let solve 
  ?is_finished:(is_finished=is_empty)
  ?extract:(extract=extract_stack) 
  cs box =

  let sols = ref [] in
  let ctx = { last=0; sp=None; } in
  let dq = append ((cs,ctx),box) Deque.empty in

  let rec loop n dq =
    if is_finished n dq then dq
    else begin
      let ((cs,ctx),box), dq = extract dq in
if !Util.debug then Format.printf "@.extract: %a@." Box.print box;
      let r = contract cs box in
(*if !Util.debug then Format.printf "contracted: %a@." Box.print box;*)
      let dq = match r, select_bb box ctx with
      | Proved, _ -> 
          sols := box::(!sols);
          dq
      | _, None -> 
          if not (Box.is_empty box) then 
            sols := box::(!sols);
          dq
      | _, Some v ->
          let b1,b2 = split v box in
          let dq = append ((cs, ctx), b1) dq in
          let dq = append ((cs, clone_ctx ctx), b2) dq in dq
      in
      loop (n+1) dq
    end
  in 
  let dq = loop 0 dq in

  (* move the boxes left in dq to sols *)
  let l = Deque.to_list dq in
  sols := List.append !sols (snd (List.split l));

  !sols
