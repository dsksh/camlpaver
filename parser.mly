%{
  open Model_common  
  open Ptree
  open Interval

  let create_id name idx = String.concat "" [name; "["; string_of_int idx; "]"]

  let env = Hashtbl.create 8
  let scope = Hashtbl.create 8
  let dom = ref []

  let add_var id d = 
    Hashtbl.add env id (Pvar id); 
    Hashtbl.add scope id (List.length !dom);
    dom := List.append !dom [d]

  let rec add_vars id n d = 
    if n > 0 then begin 
      let id_ = create_id id (n-1) in
      add_var id_ d;
      add_vars id (n-1) d end

  let get_var id = Hashtbl.find env id

  let loc () = symbol_start_pos (), symbol_end_pos ()

  let zero = loc (), (Pfloat 0.)
  let one  = loc (), (Pfloat 1.)

%}

/* tokens */

%token <int> INT
%token <float> FLOAT
%token <string> ID

/* keywords */

%token CONST
%token VAR
%token CONSTR
%token IN
%token END

%token LP
%token RP
%token LB
%token RB
%token COM
%token SCOL

/* formula expression tokens */

%token EQ
%token LE
%token LT
%token GE
%token GT

%token MIN
%token PLUS
%token MUL
%token DIV
%token POW

%token SQRT
%token EXP
%token LOG
%token SIN
%token COS
%token ATAN
%token ASIN
%token ACOS

%token INF

%token EOF

/**/

/*
%right IMP
%right AND
%right OR
%nonassoc NOT
*/
%left PLUS MIN
%left MUL DIV
%nonassoc SQRT EXP LOG SIN COS ATAN ASIN ACOS
%nonassoc UMIN
%nonassoc POW

/**/

%start main
%type <(string,int) Hashtbl.t * Interval.t list * Ptree.constr list> main 

%%

main :
  | CONST constants
    VAR variables
    CONSTR constrs 
    END EOF                 { scope, !dom, $6 } 
  | VAR variables
    CONSTR constrs 
    END EOF                 { scope, !dom, $4 } 
  ; 

/**/

constants :
  | ID EQ signed_number SCOL constants
                            { Hashtbl.add env $1 (Pfloat $3) }
  | ID EQ interval SCOL constants
                            { Hashtbl.add env $1 (Pinterval $3) }
  | ID EQ MIN interval SCOL constants
                            { Hashtbl.add env $1 (Pinterval (Interval.zero -$ $4)) }
  |                         { }

/**/

variables :
  | ID IN interval SCOL variables 
                            { add_var $1 $3 }
  | ID LB INT RB IN interval SCOL variables 
                            { add_vars $1 $3 $6 }
  |                         { }
  ;

interval :
  | LB signed_number COM signed_number RB { {inf=$2; sup=$4} }
  ;

signed_number :
  | FLOAT                   { $1 }
  | INT                     { float_of_int $1 }
  | INF                     { infinity }
  | MIN FLOAT               { -. $2 }
  | MIN INT                 { float_of_int (- $2) }
  | MIN INF                 { neg_infinity }
  ;

/**/

constrs :
  | constr SCOL constrs     { $1::$3 }
  |                         { [] }
  ;

constr :
  | expr EQ expr            { loc (), (Oeq,$1,$3) }
  | expr LT expr            { loc (), (Olt,$1,$3) }
  | expr LE expr            { loc (), (Ole,$1,$3) }
  | expr GT expr            { loc (), (Ogt,$1,$3) }
  | expr GE expr            { loc (), (Oge,$1,$3) }
  ;

/**/

expr :
  | expr PLUS expr          { loc (), Papp2 (Oadd,$1,$3) }
  | expr MIN  expr          { loc (), Papp2 (Osub,$1,$3) }
  | expr MUL  expr          { loc (), Papp2 (Omul,$1,$3) }
  | expr DIV  expr          { loc (), Papp2 (Odiv,$1,$3) }
  | SQRT LP expr RP         { loc (), Papp1 (Osqrt,$3) }
  | EXP LP expr RP          { loc (), Papp1 (Oexp,$3) }
  | LOG LP expr RP          { loc (), Papp1 (Olog,$3) }
  | SIN LP expr RP          { loc (), Papp1 (Osin,$3) }
  | COS LP expr RP          { loc (), Papp1 (Ocos,$3) }
  | ATAN LP expr RP         { loc (), Papp1 (Oatan,$3) }
  | ASIN LP expr RP         { loc (), Papp1 (Oasin,$3) }
  | ACOS LP expr RP         { loc (), Papp1 (Oacos,$3) }
  | MIN expr %prec UMIN     { loc (), Papp2 (Osub,zero,$2) }
  | expr POW INT            { loc (), Ppow ($3,$1) }
  | LP expr RP              { $2 }
  | ident_ref               { loc (), $1 }
  | const                   { loc (), $1 }
  ;

ident_ref :
  | ID                      { get_var $1 }
  | ID LB INT RB            { get_var (create_id $1 $3) }
  ;

const :
  | FLOAT                   { Pfloat $1 }
  | INT                     { Pfloat (float_of_int $1) }
  | INF                     { Pfloat infinity }
  | interval                { Pinterval $1 }
  ;

