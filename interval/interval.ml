
type t = { inf: float; sup: float }

let zero = {inf=0.; sup=0.}
let one  = {inf=1.; sup=1.}
let whole = {inf=neg_infinity; sup=infinity}
let positive = {inf=0.; sup=infinity}
let negative = {inf=neg_infinity; sup=0.}

external intv_add: float -> float -> float -> float -> t = "intv_add"
external intv_sub: float -> float -> float -> float -> t = "intv_sub"
external intv_mul: float -> float -> float -> float -> t = "intv_mul"
external intv_div: float -> float -> float -> float -> t = "intv_add"
external intv_pow: float -> float -> int -> t = "intv_pow"
external intv_sqrt: float -> float -> t = "intv_sqrt"
external intv_exp: float -> float -> t = "intv_exp"
external intv_log: float -> float -> t = "intv_log"
external intv_sin: float -> float -> t = "intv_sin"
external intv_cos: float -> float -> t = "intv_cos"
external intv_tan: float -> float -> t = "intv_tan"
external intv_asin: float -> float -> t = "intv_asin"
external intv_acos: float -> float -> t = "intv_acos"
external intv_atan: float -> float -> t = "intv_atan"
external intv_str_of: float -> float -> string = "intv_str_of"

let%test _ = intv_add 1. 2. 3. 4. = {inf=4.; sup=6.}

let (+$) x y = intv_add x.inf x.sup y.inf y.sup
let (-$) x y = intv_sub x.inf x.sup y.inf y.sup
let ( *$) x y = intv_mul x.inf x.sup y.inf y.sup
let (/$) x y = intv_div x.inf x.sup y.inf y.sup

let pow x e = intv_pow x.inf x.sup e

let sqrt x = intv_sqrt x.inf x.sup
let exp x = intv_exp x.inf x.sup
let log x = intv_log x.inf x.sup

let sin x = intv_sin x.inf x.sup
let cos x = intv_cos x.inf x.sup
let tan x = intv_tan x.inf x.sup
let asin x = intv_asin x.inf x.sup
let acos x = intv_acos x.inf x.sup
let atan x = intv_atan x.inf x.sup

let string_of_intv x = intv_str_of x.inf x.sup

let print fmt x = Format.fprintf fmt "%s" (string_of_intv x)

