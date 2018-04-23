
open Interval

let sample_mp x = Interval.mid x
let sample_inf x = max x.inf neg_infinity
let sample_sup x = min x.sup infinity

let step ?sample_fun:(sample_fun=sample_mp) fn vn box =
  let f,d = fn in
  let d = List.nth d (Box.ind_of box vn) in
  let v = Box.get box vn in

  let v_f = Expr.eval box f in
  if is_empty v_f || not (is_superset v_f zero) then empty
  else
  let v_d = Expr.eval box d in
  if is_empty v_d then empty
  else begin
    let c = sample_fun v in
    Box.set box vn (of_float c);
    let v_c = Expr.eval box f in
    Box.set box vn v;

    let l,r = ext_div v_c v_d in
    let l = (of_float c) -$ l in
    let r = (of_float c) -$ r in
    let l = intersect l v in
    let r = intersect r v in

    if is_empty l then r 
    else join l r
  end

let contract ?sample_fun:(sf=sample_mp) fn vn box =
  let rec loop () =
    let old = Box.get box vn in
    Box.set box vn (step ~sample_fun:sf fn vn box);
    if not (Box.get box vn = old) || not (Box.is_empty box) then
      loop ()
  in
  loop ()

