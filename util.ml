open Format

let debug = ref false

type error =
  | UnknownId of string
  | Unsupported of string
  | ZeroDivision
  | Unexpected

exception Error of error

let report fmt = function
  | UnknownId id ->
      fprintf fmt "id %s is unknown" id
  | Unsupported desc ->
      fprintf fmt "%s is unsupported" desc
  | ZeroDivision ->
      fprintf fmt "division by zero"
  | Unexpected->
      fprintf fmt "unexpected error"

let error e = raise (Error e)

