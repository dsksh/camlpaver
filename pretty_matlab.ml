open Format
open Interval
open Box

let print_vs fmt v_tbl =
  fprintf fmt "vs = { ";
  let pr vn _ = fprintf fmt "'%s'; " vn in
  let _ = Hashtbl.iter pr v_tbl in
  fprintf fmt "};@."

let print_box v_tbl fmt s_id box =
  (* printer for a var name *)
  let pr vn sz v_id =
    fprintf fmt "data(%d,%d).name = '%s';@." (s_id+1) v_id vn;
    fprintf fmt "data(%d,%d).value = [" (s_id+1) v_id;

    (* printer for a step *)
    let rec loop s =
      if sz < 0 then
        let itv = Box.get box vn in
        fprintf fmt "%f %f; " itv.inf itv.sup
      else if s < sz then
        let vn_ = String.concat "" [vn; "["; string_of_int s; "]"] in
        let itv = Box.get box vn_ in
        fprintf fmt "%f %f; " itv.inf itv.sup;
        loop (s+1)
    in
    loop 0;
    fprintf fmt "];@.";
    v_id+1
  in
  let _ = Hashtbl.fold pr v_tbl 1 in ()

let print_sols fmt = function
  | s::rest as sols -> 
      (* create variable table *)
      let vn_l = s.s.vn_list in
      let v_tbl = Hashtbl.create (List.length vn_l) in
      let parse_vn vn =
        try 
          let il = String.index vn '[' in
          let ir = String.index vn ']' in
          let ind = int_of_string (String.sub vn (il+1) (ir-il-1)) in
          let vn = String.sub vn 0 il in
          if Hashtbl.mem v_tbl vn then
            (if ind > Hashtbl.find v_tbl vn then
              Hashtbl.replace v_tbl vn ind)
          else
            Hashtbl.add v_tbl vn ind
        with Not_found -> 
            Hashtbl.add v_tbl vn (-1)
      in
      let _ = List.map parse_vn vn_l in

      (* print *)
      fprintf fmt "function [vs, data] = solution()@.";
      print_vs fmt v_tbl;
      let _ = List.mapi (print_box v_tbl fmt) sols in
      fprintf fmt "end@.";
      ()
  | [] -> ()
