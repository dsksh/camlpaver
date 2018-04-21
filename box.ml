
module Scope = struct

  type t = {
    map : (string,int) Hashtbl.t;
    dom : Interval.t array;
  }
  
  let create map dom =
    { map = map; dom = Array.of_list dom; }

  let to_var_list t =
    let vs = Array.make (Hashtbl.length t.map) "" in
    let a n i = vs.(i) <- n in
    Hashtbl.iter a t.map;
    Array.to_list vs

  let print fmt t =
    let pr n i = Format.fprintf fmt "%s in %a;@;" n Interval.print (t.dom.(i)) in
    Format.fprintf fmt "@[";
    Hashtbl.iter pr t.map;
    Format.fprintf fmt "@]"

end

open Scope

type t = Interval.t array

let make sc = Array.make (Hashtbl.length sc.map) Interval.zero

let print fmt (sc, t) =
  let pr n i = Format.fprintf fmt "%s in %a;@;" n Interval.print (t.(i)) in
  Format.fprintf fmt "@[";
  Hashtbl.iter pr sc.map;
  Format.fprintf fmt "@]"

