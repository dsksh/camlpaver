
type t = { mutable last : int; mutable sp : int option; }

let eps = ref 1e-4

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

let contract cs box =
  if not (Box.is_empty box) then
    let ctr c = Contractor_hull.contract c box in
    let _ = List.map ctr cs in 

    let ctr c = 
      let ctr_ vn =
        let t = Contractor_box.init c vn box in
        Contractor_box.contract t in
      let _ = List.map ctr_ (Box.get_vn_list box) in ()
    in
    let _ = List.map ctr cs in 

    ()

let split vn box =
  let v0 = Box.get box vn in
  let b1 = box in
  let b2 = Box.copy box in
  let m = Interval.mid v0 in
  Box.set b1 vn (Interval.make v0.inf m);
  Box.set b2 vn (Interval.make m v0.sup);
  b1, b2

let clone_ctx ctx = { last=ctx.last; sp=ctx.sp; }

let solve ?extract:(extract=extract_stack) cs box =
  let sols = ref [] in
  let ctx = { last=0; sp=None; } in
  let dq = append (box,ctx) Deque.empty in

  let rec loop dq =
    if not (Deque.is_empty dq) then
      let (box,ctx), dq = extract dq in
Format.printf "@.extract: %a@." Box.print box;
      contract cs box;
      match select_bb box ctx with
      | None -> 
          if not (Box.is_empty box) then 
            sols := box::(!sols);
          loop dq
      | Some v ->
          let b1,b2 = split v box in
          let dq = append (b1, ctx) dq in
          let dq = append (b2, clone_ctx ctx) dq in
          loop dq
  in 
  loop dq;
  !sols
