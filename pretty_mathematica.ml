
open Format
open Box
open Interval

let print_intv fmt intv =
  fprintf fmt "Interval[{%f,%f}]" intv.inf intv.sup

let print_box fmt box =
  let f = ref true in
  let pr vn = 
    if !f then f := false else fprintf fmt ",@;";
    let i = Hashtbl.find box.s.table vn in
    fprintf fmt "\"%s\" -> %a" vn print_intv (box.v.(i)) in
  fprintf fmt "<|";
  let _ = List.map pr box.s.vn_list in
  fprintf fmt "|>"

