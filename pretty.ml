open Format
open Ptree
open Model_common
open Expr
open Constr
open Hashcons

(* printers for Ptree *)

let rec print_pexpr fmt = function 
  | _, Pvar id -> 
      fprintf fmt "%s" id
  | _, Pfloat v ->
      fprintf fmt "%f" v
  | _, Pintv v ->
      fprintf fmt "%a" Interval.print v
  | _, Papp1 (op,expr) ->
      fprintf fmt "%s(%a)" (str_of_op1 op) print_pexpr expr
  | _, Papp2 (op,e1,e2) ->
      fprintf fmt "(%a %s %a)" print_pexpr e1 (str_of_op2 op) print_pexpr e2
  | _, Ppow (n,e) ->
      fprintf fmt "%a^%d" print_pexpr e n

let rec print_pconstr fmt = function
  | _, Prel (op,e1,e2) ->
      fprintf fmt "%a %s %a" print_pexpr e1 (str_of_rop op) print_pexpr e2
  | _, Pif (c1,c2) ->
      let pr c = print_pconstr fmt c in
      fprintf fmt "@[if (";
      let _ = List.map pr c1 in
      fprintf fmt ")@;then (";
      let _ = List.map pr c2 in
      fprintf fmt ")@]"
  | _, PifElse (c1,c2,c3) ->
      let pr c = print_pconstr fmt c in
      fprintf fmt "@[if (";
      let _ = List.map pr c1 in
      fprintf fmt ")@;then (";
      let _ = List.map pr c2 in
      fprintf fmt ")@;else (";
      let _ = List.map pr c3 in
      fprintf fmt ")@]"


(* printers for Expr *)

let rec print_expr fmt expr = 
  match expr.node with
  | Var id -> 
      fprintf fmt "%s" id
  | Val v -> 
      fprintf fmt "%a" Interval.print v
  | App1 (op,expr) ->
      fprintf fmt "%s(%a)" (str_of_op1 op) print_expr expr
  | App2 (op,e1,e2) ->
      fprintf fmt "(%a %s %a)" print_expr e1 (str_of_op2 op) print_expr e2
  | Pow (n,e) ->
      fprintf fmt "%a^%d" print_expr e n

let print_dual fmt = function
  | e, ds -> 
      fprintf fmt "@[<2>";
      print_expr fmt e;
      fprintf fmt "@;@[<2>{@;";
      let pr d = fprintf fmt "%a;@;" print_expr d in
      let _ = List.map pr ds in
      fprintf fmt "}@]@,@]"

(* for Constr *)

let rec print_constr fmt constr = 
  match constr.node with
  | C (op,e1,e2) -> fprintf fmt "@[%a@;%s@;%a@]"
      print_expr (fst e1) (str_of_rop op) print_expr (fst e2)
  | G (c1,c2) -> fprintf fmt "(%a ==> %a)" print_constr c1 print_constr c2
  | P (c1,c2) -> 
      fprintf fmt "(%a,@,%a)" print_constr c1 print_constr c2
  | True -> fprintf fmt "true"
