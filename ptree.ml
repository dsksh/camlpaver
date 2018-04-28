open Format
open Model_common

type loc = Lexing.position * Lexing.position

type id = loc * ident
type lid = loc * ident

type expr = loc * expr_node
and  expr_node =
  | Pvar of ident
  | Pfloat of float
  | Pintv of Interval.t
  | Papp1 of un_op * expr
  | Papp2 of bin_op * expr * expr
  | Ppow of int * expr

type constr = loc * constr_node
and  constr_node = 
  | Prel of rel_op * expr * expr
  | Pif of constr list * constr list
  | PifElse of constr list * constr list * constr list
