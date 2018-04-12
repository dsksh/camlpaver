{
  open Lexing
  open Parser
    
  exception Lexical_error of string

  let id_or_keyword = 
    let h = Hashtbl.create 2 in
    List.iter (fun (s,k) -> Hashtbl.add h s k)
	[ "sqrt", SQRT;
	  "exp", EXP;
	  "log", LOG;
	  "sin", SIN;
	  "cos", COS;
	  "atan", ATAN;
	  "asin", ASIN;
	  "acos", ACOS;

	  "inf", INF;

	  "constants", CONST;
	  "Constants", CONST;
    "variables", VAR;
    "Variables", VAR;
    "constraints", CONSTR;
    "Constraints", CONSTR;

    "in", IN;

    "end", END;

  ];
  fun s -> 
  try Hashtbl.find h s with Not_found -> ID s

  let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- 
      { pos with pos_lnum = pos.pos_lnum + 1; 
	    pos_bol = pos.pos_cnum + 1 }
}

let delim  = [' ' '\t' '\r']
let ws     = delim+
let digit  = ['0'-'9']
let letter = ['a'-'z' 'A'-'Z']
let ident  = letter (letter | digit | '_')*
let number = digit+

let mk     = '.'
let exp    = ('e' | 'E')
let plus   = '+'
let min    = '-'
let sign   = (plus|min)
let float1 = digit+ mk digit*
let float2 = digit+ mk digit* exp digit+
let float3 = digit+ mk digit* exp sign digit+
let float4 = digit+ exp digit+
let float5 = digit+ exp sign digit+

rule token = parse
  | '\n' 
      { newline lexbuf; token lexbuf }
  | ws
      { token lexbuf }
  | ident
      { id_or_keyword (Lexing.lexeme lexbuf) }

  | float1
      { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | float2
      { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | float3
      { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | float4
      { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | float5
      { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | number
      { INT (int_of_string (Lexing.lexeme lexbuf)) }

  | "/*/"
      { (*LP_STAR_RP*) token lexbuf }
  | "/*"
      { (*comment_start_loc := loc lexbuf;*) comment lexbuf; token lexbuf }

  | "#" { lcomment lexbuf } (* TODO *)

  | "="  { EQ }
  | "<=" { LE }
  | "<"  { LT }
  | ">=" { GE }
  | ">"  { GT }

  | "("  { LP }
  | ")"  { RP }
  | "["  { LB }
  | "]"  { RB }
  | ","  { COM }
  | ";"  { SCOL }

  | min  { MIN }
  | plus { PLUS }
  | "*"  { MUL }
  | "/"  { DIV }
  | "^"  { POW }

  | eof  { EOF }

  | _ 
      { raise (Lexical_error ("illegal character: " ^ lexeme lexbuf)) }

and comment = parse
  | "/*/"
      { comment lexbuf }
  | "*/"
      { () }
  | "/*"
      { comment lexbuf; comment lexbuf }
  | '\n'
      { newline lexbuf; comment lexbuf }
  | eof
      { raise (Lexical_error ("unterminated comment")) }
  | _
      { comment lexbuf }

and lcomment = parse
  | '\n' { newline lexbuf; token lexbuf }
  | _    { lcomment lexbuf }
  | eof  { EOF }
