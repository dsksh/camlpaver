
type t = { inf: float; sup: float; }

let make inf sup = { inf=inf; sup=sup; }

let of_float x = { inf=x; sup=x }
let of_int x = of_float (float_of_int x)

let zero  = of_float 0.
let one   = of_float 1.
let whole = make neg_infinity infinity
let positive = make 0. infinity
let negative = make neg_infinity 0.

let empty = of_float nan
let is_nan f = f <> f
let is_empty x = (is_nan x.inf && is_nan x.sup)

type p = float*float

external intv_add: float -> float -> float -> float -> p = "intv_add"
external intv_sub: float -> float -> float -> float -> p = "intv_sub"
external intv_mul: float -> float -> float -> float -> p = "intv_mul"
external intv_div: float -> float -> float -> float -> p = "intv_div"

let intv_div_ inf1 sup1 inf2 sup2 =
  if inf2 <= 0. && 0. <= sup2 then
    failwith "div by zero!";
  intv_div inf1 sup1 inf2 sup2

external intv_width: float -> float -> float = "intv_width"
external intv_rad:  float -> float -> float = "intv_rad"
external intv_mid:  float -> float -> float = "intv_mid"
external intv_norm: float -> float -> float = "intv_norm"

external intv_pow:  float -> float -> int -> p = "intv_pow"
external intv_sqrt: float -> float -> p = "intv_sqrt"
external intv_exp:  float -> float -> p = "intv_exp"
external intv_log:  float -> float -> p = "intv_log"
external intv_sin:  float -> float -> p = "intv_sin"
external intv_cos:  float -> float -> p = "intv_cos"
external intv_tan:  float -> float -> p = "intv_tan"
external intv_asin: float -> float -> p = "intv_asin"
external intv_acos: float -> float -> p = "intv_acos"
external intv_atan: float -> float -> p = "intv_atan"
external intv_abs:  float -> float -> p = "intv_abs"
external intv_str_of: float -> float -> string = "intv_str_of"

(*let%test _ = intv_add 1. 2. 3. 4. = (4.,6.)*)

let t_of_p p = make (fst p) (snd p)

let (+$)  x y = t_of_p (intv_add x.inf x.sup y.inf y.sup)
let (-$)  x y = t_of_p (intv_sub x.inf x.sup y.inf y.sup)
let ( *$) x y = t_of_p (intv_mul x.inf x.sup y.inf y.sup)
let (/$)  x y = t_of_p (intv_div_ x.inf x.sup y.inf y.sup)

let width x = intv_width x.inf x.sup
let rad  x = intv_rad x.inf x.sup
let mid  x = intv_mid x.inf x.sup
let norm x = intv_norm x.inf x.sup

let abs  x = t_of_p (intv_abs x.inf x.sup)

let pow  x n = t_of_p (intv_pow x.inf x.sup n)

let sqrt x = t_of_p (intv_sqrt x.inf x.sup)
let exp  x = t_of_p (intv_exp  x.inf x.sup)
let log  x = t_of_p (intv_log  x.inf x.sup)

let sin  x = t_of_p (intv_sin  x.inf x.sup)
let cos  x = t_of_p (intv_cos  x.inf x.sup)
let tan  x = t_of_p (intv_tan  x.inf x.sup)
let asin x = t_of_p (intv_asin x.inf x.sup)
let acos x = t_of_p (intv_acos x.inf x.sup)
let atan x = t_of_p (intv_atan x.inf x.sup)

let string_of_intv x = intv_str_of x.inf x.sup
let print fmt x = Format.fprintf fmt "%s" (string_of_intv x)

(* utility functions *)

let (~-$) x = zero -$ x

let pow_intv x y =
  exp (y *$ (log x))

let min x y =
  if x <> x (* = nan *) then y
  else (* <> nan *)
    if x > y then y (* <> nan *) else x

let max x y =
  if x <> x (* = nan *) then y
  else (* <> nan *)
    if x < y then y (* <> nan *) else x

let intersect x y =
    let l = max x.inf y.inf in
    let u = min x.sup y.sup in
    if l <= u then { inf=l; sup=u } else empty

let join x y =
    let l = min x.inf y.inf in
    let u = max x.sup y.sup in
    { inf=l; sup=u }

let is_contained x f =
  x.inf <= f && f <= x.sup

let is_superset x y =
  x.inf <= y.inf && y.sup <= x.sup

let is_strict_superset x y =
  x.inf < y.inf && y.sup < x.sup

let distance x y =
  if x == empty || y == empty then nan
  else 
    (* TODO: cases of infinity bounds *)
    max (abs_float (x.inf -. y.inf)) (abs_float (x.sup -. y.sup))

let ext_div x y =
  if is_superset y zero then begin
    if x.inf > 0. then
      let xl = of_float x.inf in
      let yl = of_float (min y.inf (-. min_float)) in (* TODO *)
      let yu = of_float (max y.sup min_float) in
      let xl_yl = xl /$ yl in
      let xl_yu = xl /$ yu in
      let r1 = make neg_infinity xl_yl.sup in
      let r2 = make xl_yu.inf infinity in
      r1, r2
    else if x.sup < 0. then
      let xu = of_float x.sup in
      let yl = of_float (min y.inf (-. min_float)) in
      let yu = of_float (max y.sup min_float) in
      let xu_yu = xu /$ yu in
      let xu_yl = xu /$ yl in
      let r1 = make neg_infinity xu_yu.sup in
      let r2 = make xu_yl.inf infinity in
      r1, r2
    else 
      whole, empty
  end else
    x /$ y, empty

let rec root x n =
  if is_empty x then x
  else if x.inf = 0. && x.sup = 0. then
    zero
  else if n = 0 then 
    one
  else if n < 0 then
    one /$ (root x (-n))
  else if n = 1 then 
    x
  else if n mod 2 = 0 then
    pow_intv x (one /$ (of_int n))
  else
    join (pow_intv x (one /$ (of_int n)))
      (~-$ (pow_intv (zero -$ x) (one /$ (of_int n))))

let slice_lower ?eps:(eps=1e-8) x =
  if is_empty x || width x <= eps then x
  else
    let b = (max (-. max_float) x.inf) +. eps in
    if not (is_superset x (of_float b)) then x
    else make x.inf b

let slice_upper ?eps:(eps=1e-8) x =
  if is_empty x || width x <= eps then x
  else
    let b = (min max_float x.sup) -. eps in
    if not (is_superset x (of_float b)) then x
    else make b x.sup
