
type t = { inf: float; sup: float; }

let intv_of_float x = { inf=x; sup=x }
let intv_of_int x = intv_of_float (float_of_int x)

let zero  = { inf=0.; sup=0.; }
let one   = { inf=1.; sup=1.; }
let whole = { inf=neg_infinity; sup=infinity; }
let positive = { inf=0.; sup=infinity; }
let negative = { inf=neg_infinity; sup=0.; }

let empty = { inf=nan; sup=nan; }
let is_empty x = x = empty

type p = float*float

external intv_add: float -> float -> float -> float -> p = "intv_add"
external intv_sub: float -> float -> float -> float -> p = "intv_sub"
external intv_mul: float -> float -> float -> float -> p = "intv_mul"
external intv_div: float -> float -> float -> float -> p = "intv_add"

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

let t_of_p p = { inf=fst p; sup=snd p; }

let (+$)  x y = t_of_p (intv_add x.inf x.sup y.inf y.sup)
let (-$)  x y = t_of_p (intv_sub x.inf x.sup y.inf y.sup)
let ( *$) x y = t_of_p (intv_mul x.inf x.sup y.inf y.sup)
let (/$)  x y = t_of_p (intv_div x.inf x.sup y.inf y.sup)

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

let pow_intv x y =
  exp (y *$ (log x))

let intersect x y =
    let l = max x.inf y.inf in
    let u = min x.sup y.sup in
    if l <= u then { inf=l; sup=u } else empty

let join x y =
    let l = min x.inf y.inf in
    let u = max x.sup y.sup in
    { inf=l; sup=u }

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
    pow_intv x (one /$ (intv_of_int n))
  else
    join 
      (pow_intv x (one /$ (intv_of_int n)))
      (zero -$ (pow_intv (zero -$ x) (one /$ (intv_of_int n))))

let string_of_intv x = intv_str_of x.inf x.sup

let print fmt x = Format.fprintf fmt "%s" (string_of_intv x)

