open Format
open Ptree
open Model_common

let rec print_expr fmt = function 
  | _, Pvar id -> 
      fprintf fmt "%s" id
  | _, Pfloat v ->
      fprintf fmt "%f" v
  | _, Pinterval (lb,ub) ->
      fprintf fmt "[%f,%f]" lb ub
  | _, Papp1 (op,expr) ->
      fprintf fmt "%s(%a)" (str_of_op1 op) print_expr expr
  | _, Papp2 (op,e1,e2) ->
      fprintf fmt "(%a %s %a)" print_expr e1 (str_of_op2 op) print_expr e2
  | _, Ppow (e,expr) ->
      fprintf fmt "%a^%d" print_expr expr e

let print_constr fmt constr = 
  let _, (op, e1, e2) = constr in
    fprintf fmt "%a %s %a" print_expr e1 (str_of_rop op) print_expr e2
