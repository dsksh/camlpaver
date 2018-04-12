
type rational = int * int

type ident = string

type un_op = Osqr | Osqrt | Oexp | Olog | Osin | Ocos | Oatan | Oasin | Oacos

type bin_op = Oadd | Osub | Omul | Odiv

type rel_op = Oeq | Olt | Ole | Ogt | Oge

let str_of_op1 = function
  | Osqr -> "sqr"
  | Osqrt -> "sqrt"
  | Oexp -> "exp"
  | Olog -> "log"
  | Osin -> "sin"
  | Ocos -> "cos"
  | Oatan -> "atan"
  | Oasin -> "asin"
  | Oacos -> "acos"

let str_of_op2 = function
  | Oadd -> "+"
  | Osub -> "-"
  | Omul -> "*"
  | Odiv -> "/" 

let str_of_rop = function
  | Oeq -> "="
  | Olt -> "<"
  | Ole -> "<="
  | Ogt -> ">" 
  | Oge -> ">=" 


module MEnv = Map.Make(String)
module MDom = Map.Make(String)
