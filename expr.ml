open Hashcons
open Model_common
open Ptree
open Util

(* hash-consed expression type *)

type t = node hash_consed
and  node =
  | Var of ident
  | Val of Interval.t
  | App1 of un_op * t
  | App2 of bin_op * t * t
  | Pow of int * t

type dual = t * t list

module Expr_node = struct
  type t = node

  let equal e1 e2 = 
    match e1, e2 with
    | Var n1, Var n2 -> n1 == n2
    | Val v1, Val v2 -> v1 = v2
    | App1 (op1,e1), App1 (op2,e2) -> op1 == op2 && e1 == e2
    | App2 (op1,e1,f1), App2 (op2,e2,f2) -> op1 == op2 && e1 == e2 && f1 == f2
    | Pow (i1,e1), Pow (i2,e2) -> i1 == i2 && e1 == e2
    | _ -> false

  (* TODO *)
  let hash = function
    | Var n -> Hashtbl.hash n
    | Val v -> abs (19*(Hashtbl.hash (int_of_float v.inf)) + 
        (Hashtbl.hash (int_of_float v.sup)) + 1 ) 
    | App1 (op,e) -> abs (19*e.hkey + (match op with
        | Osqr -> 1 | Osqrt -> 2 | Oexp -> 3| Olog -> 4 
        | Osin -> 5 | Ocos -> 6 | Otan -> 7
        | Oasin -> 8 | Oacos -> 9 | Oatan -> 10 )
        + 1 )
    | App2 (op,e1,e2) -> abs (19*(19*e1.hkey + e2.hkey) + 
        (match op with | Oadd -> 1 | Osub -> 2 | Omul -> 3 | Odiv -> 4)
        + 2 )
    | Pow (i,e) -> abs (19*(Hashtbl.hash i) + e.hkey + 1)
end

module Hexpr = Make(Expr_node)

let ht = Hexpr.create 251

let get_ht_size () = 
  let _,_,sz,_,_,_ = (Hexpr.stats ht) in 
  sz

let get_max_tag () =
  let e = Hexpr.hashcons ht (Val Interval.empty) in
  e.tag

(* constructors for the expressions *)

let mk_var n = Hexpr.hashcons ht (Var n)
let mk_val v = Hexpr.hashcons ht (Val v)

let mk_app1 op e = match op,e.node with
  | (Osqr|Osqrt|Osin),Val z when z = Interval.zero -> e
  | (Oexp|Ocos),Val z when z = Interval.zero ->
      Hexpr.hashcons ht (Val Interval.one)
  | (Osqr|Osqrt),Val z when z = Interval.one -> e
  | Olog,Val z when z = Interval.one ->
      Hexpr.hashcons ht (Val Interval.zero)
  | _ -> Hexpr.hashcons ht (App1 (op,e))

let mk_app2 op e1 e2 = match op,e1.node,e2.node with
  | Oadd,Val z,_  when z = Interval.zero -> e2
  | (Oadd|Osub),_,Val z when z = Interval.zero -> e1
  | Odiv,_,Val z when Interval.is_contained z 0. -> error ZeroDivision
  | Omul,Val z,_ when z = Interval.one -> e2
  | (Omul|Odiv),_,Val z when z = Interval.one -> e1
  | (Omul|Odiv),Val z,_ when z = Interval.zero -> e1
  | Omul,_,Val z when z = Interval.zero -> e2
  | _ -> Hexpr.hashcons ht (App2 (op,e1,e2))

let mk_pow i e = match i,e.node with
  | 0,_ -> Hexpr.hashcons ht (Val Interval.one)
  | 1,_ -> e
  | i,_ -> Hexpr.hashcons ht (Pow (i,e))

let rec mk_expr = function
  | _, Pvar id -> mk_var id
  | _, Pfloat v -> mk_val (Interval.of_float v)
  | _, Pintv v -> mk_val v
  | _, Papp1 (op,e) -> mk_app1 op (mk_expr e)
  | _, Papp2 (op,e1,e2) -> mk_app2 op (mk_expr e1) (mk_expr e2)
  | _, Ppow (i,e) -> mk_pow i (mk_expr e)


(* constructors for the derivatives *)

let rec diff_expr vid t =
  let diff = diff_expr vid in
  match t.node with
  | Var id -> if id = vid then mk_val Interval.one else mk_val Interval.zero
  | Val _ -> mk_val Interval.zero

  | App1 (Osqr,e) ->
      (mk_app2 Omul (diff e) (mk_app2 Omul (mk_val (Interval.of_float 2.)) e))
  | App1 (Osqrt,e) ->
      (mk_app2 Odiv (diff e) (mk_app2 Omul 
        (mk_val (Interval.of_float 2.)) (mk_app1 Osqrt e) ))
  | App1 (Oexp,e) ->
      (mk_app2 Omul (mk_app1 Oexp e) (diff e))
  | App1 (Olog,e) ->
      (mk_app2 Odiv (diff e) e)
  | App1 (Osin,e) ->
      (mk_app2 Omul (mk_app1 Ocos e) (diff e))
  | App1 (Ocos,e) ->
      (mk_app2 Omul (mk_app2 Osub (mk_val Interval.zero) 
        (mk_app1 Osin e)) (diff e) )
  | App1 (Oatan,e) ->
      (mk_app2 Omul (mk_app2 Odiv (mk_val Interval.one) 
        (mk_app2 Oadd (mk_val Interval.one) (mk_app1 Osqr e)) ) (diff e) )
  | App1 (op,_) -> error (Unsupported (str_of_op1 op))

  | App2 (Oadd,e1,e2) ->
      (mk_app2 Oadd (diff e1) (diff e2))
  | App2 (Osub,e1,e2) ->
      (mk_app2 Osub (diff e1) (diff e2))
  | App2 (Omul,e1,e2) ->
      (mk_app2 Oadd (mk_app2 Omul (diff e1) e2) (mk_app2 Omul e1 (diff e2)))
  | App2 (Odiv,e1,e2) ->
      (mk_app2 Odiv (mk_app2 Osub (mk_app2 Omul (diff e1) e2) 
        (mk_app2 Omul e1 (diff e2))) (mk_app1 Osqr e2) )

  | Pow (n,e) ->
      let n_ = mk_val (Interval.of_int n) in
      if n = 3  then
        (mk_app2 Omul (diff e) (mk_app2 Omul n_ (mk_app1 Osqr e)))
      else
        (mk_app2 Omul (diff e) (mk_app2 Omul n_ (mk_pow (n-1) e)))

let mk_dual_expr vs e =
  let e = mk_expr e in
  let de = List.map (fun v -> diff_expr v e) vs in
  (e, de)


(* make subtraction expression lhs - rhs *)
let mk_diff_expr vs (e1,_) (e2,_) =
  let e = mk_app2 Osub e1 e2 in
  let de = List.map (fun v -> diff_expr v e) vs in
  (e, de)


(* evaluation *)

let rec eval box t = 
  match t.node with
  | Var n -> Box.get box n
  | Val v -> v
  | App1 (op,e) -> 
      let v = eval box e in
      (impl_of_op1 op) v
  | App2 (op,e1,e2) ->
      let v1 = eval box e1 in
      let v2 = eval box e2 in
      (impl_of_op2 op) v1 v2
  | Pow (n,e) ->
      let v = eval box e in
      Interval.pow v n
