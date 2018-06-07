
module Scope = struct

  type t = {
    table : (string,int) Hashtbl.t; (* from var name to var index *)
    vn_list : string list; (* list of var names sorted by indices *)
    iv : Interval.t array; (* initial domain *)
  }
  
  let make table iv_l =
    let vn_a = Array.make (Hashtbl.length table) "" in
    let a vn i = vn_a.(i) <- vn in
    Hashtbl.iter a table;
    let vn_list = Array.to_list vn_a in
    { table = table; vn_list = vn_list; iv = Array.of_list iv_l; }

end

open Scope

type t = {
  s : Scope.t;
  v : Interval.t array; (* data of boxes *)
}

let make s = 
  let v = Array.copy s.iv in
  { s = s; v = v; }

let empty s = 
  let v = Array.make (Hashtbl.length s.table) Interval.empty in
  { s = s; v = v; }

let copy t = 
  let v = Array.copy t.v in
  { s = t.s; v = v; }

let ind_of t vn =
  Hashtbl.find t.s.table vn

let get t vn =
  t.v.(ind_of t vn)

let set t vn v =
  t.v.(ind_of t vn) <- v

let get_vn_list t = t.s.vn_list

let get_vn t idx = List.nth t.s.vn_list idx

let get_index t vn = Hashtbl.find t.s.table vn

let dim t =
  Hashtbl.length t.s.table

let width t =
  let r = ref 0. in
  let f v = if Interval.width v > !r then r := Interval.width v in
  Array.iter f t.v;
  !r

let is_empty t =
  let r = ref false in
  let f v = if Interval.is_empty v then r := true in
  Array.iter f t.v;
  !r

let print fmt t =
  let f = ref true in
  let pr vn = 
    if !f then f := false else Format.fprintf fmt ",@;";
    let i = Hashtbl.find t.s.table vn in
    Format.fprintf fmt "\"%s\" : %a" vn Interval.print (t.v.(i)) in
  Format.fprintf fmt "@[<2>{";
  let _ = List.map pr t.s.vn_list in
  Format.fprintf fmt "}@]"

