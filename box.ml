
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

  (*let print fmt t =
    let f = ref true in
    let pr vn = 
      if !f then f := false else Format.fprintf fmt ",@;";
      let i = Hashtbl.find t.table vn in
      Format.fprintf fmt "\"%s\" : %a" vn Interval.print (t.iv.(i)) in
    Format.fprintf fmt "@[<2>{";
    let _ = List.map pr t.vn_list in
    Format.fprintf fmt "}@]"
  *)

end

open Scope

type t = {
  s : Scope.t;
  v : Interval.t array; (* data of boxes *)
}

let make s = 
  let v = Array.copy s.iv in
  { s = s; v = v; }

let get t vn =
  let i = Hashtbl.find t.s.table vn in
  t.v.(i)

let set t vn v =
  let i = Hashtbl.find t.s.table vn in
  t.v.(i) <- v

let length t =
  Hashtbl.length t.s.table

let print fmt t =
  let f = ref true in
  let pr vn = 
    if !f then f := false else Format.fprintf fmt ",@;";
    let i = Hashtbl.find t.s.table vn in
    Format.fprintf fmt "\"%s\" : %a" vn Interval.print (t.v.(i)) in
  Format.fprintf fmt "@[<2>{";
  let _ = List.map pr t.s.vn_list in
  Format.fprintf fmt "}@]"

