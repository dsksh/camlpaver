(* Taken from http://codegists.com/snippet/ocaml/dequeml_ergl_ocaml *)

type 'a t = 'a list * 'a list
 
let rec take' acc n = function
  | [] -> acc
  | l::ls when n = 0 -> acc
  | l::ls -> take' (l::acc) (n-1) ls

let take n l = List.rev @@ take' [] n l

let rec drop n = function
  | [] -> []
  | l::ls when n = 0 -> l::ls
  | l::ls -> drop (n-1) ls

let checkf (f, r) =
  let size_f, size_r = List.length f, List.length r in
  let c = 4 and
      diff = (size_f + size_r) / 2 in
   
  if size_f > c * size_r + 1 then begin
    let front = take diff f and
        rear = r @ List.rev (drop diff f) in
    (front, rear)
   
  end else if size_r > c * size_f + 1 then begin
    let front = f @ List.rev (drop diff r) and
        rear = take diff r in
    (front, rear)
   
  end else (f, r)

let empty = ([], [])
 
let is_empty = function
  | ([], []) -> true
  | _ -> false

let to_list (f, r) = f @ List.rev r

let length (f, r) = List.length f + List.length r

let first = function
  | ([], [x]) -> x
  | (f, _) -> List.hd f

let last = function
  | ([x], []) -> x
  | (_, r) -> List.hd r

let cons x (f, r) = checkf (x::f, r)
let snoc x (f, r) = checkf (f, x::r)

let tail = function
  | ([], [x]) -> ([], [])
  | (_::fs, r) -> checkf (fs, r)
  | _ -> failwith "tail of empty deque"

let eject = function
  | ([x], []) -> ([], [])
  | (f, _::rs) -> checkf (f, rs)
