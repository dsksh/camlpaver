open Model_common

type t = {
  lhs : Expr.t * Expr.t list;
  box : Box.t;
  proj : Interval.t;
  mutable vn : string;
}

let init constr box =
  let op,e1,e2 = constr in
  let lhs = Expr.mk_diff_expr (Box.get_vn_list box) e1 e2 in
  let proj = match op with
  | Oeq -> Interval.zero
  | Olt | Ole -> Interval.negative
  | Ogt | Oge -> Interval.positive
  in
  { lhs=lhs; box=box; proj=proj; vn=""; }

let set_var t vn = t.vn <- vn

type r = NoSol | Proved | Unknown

