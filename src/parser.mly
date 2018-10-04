%{
  open Core
  open Location
  open Syntax

  let error ?loc code = raise (ParseError (loc, code))
%}

%token <Big_int.big_int> INT
%token <string> IDENT

%token CHECK
%token COMMA
%token EOF
%token HAT
%token PLUS
%token STAR
%token VAR

%token LPAREN RPAREN
%token LBRACE RBRACE

%left  PLUS
%left  STAR
%right HAT

%type <Syntax.pprogram> program

%start program
%%

(* -------------------------------------------------------------------- *)
program:
| e=entry* EOF
    { e }

| x=loc(error)
    { error ~loc:(loc x) PE_Unknown }

(* -------------------------------------------------------------------- *)
entry:
| e=loc(entry_r) { e }

(* -------------------------------------------------------------------- *)
entry_r:
| v=var_decl { PVarDecl v }
| t=check    { PCheck   t }

(* -------------------------------------------------------------------- *)
var_decl:
| VAR xs=seq0(ident) { xs }

(* -------------------------------------------------------------------- *)
check:
| CHECK t=term { t }

(* -------------------------------------------------------------------- *)
%inline ident:
| id=loc(IDENT) { id }

(* -------------------------------------------------------------------- *)
%inline uint:
| i=INT { i }

(* -------------------------------------------------------------------- *)
%inline binop:
| PLUS { (`Add :> pbinop) }
| STAR { (`Mul :> pbinop) }
| HAT  { (`Pow :> pbinop) }

(* -------------------------------------------------------------------- *)
term_r:
| x=ident
    { PVar x }

| l=literal
    { PLit l }

| t1=term o=loc(binop) t2=term
    { PApp (`BinOp o, [t1; t2]) }

| f=ident args=tuple0(term)
    { PApp (`Named f, args) }

(* -------------------------------------------------------------------- *)
literal_r:
| i=uint
    { PL_Int i }

| i=uint n=braces(term) {
    let ( =. ) = Big_int.eq_big_int in

    match i with
    | i when i =. Big_int.zero_big_int -> PL_BitString (false, n)
    | i when i =. Big_int.unit_big_int -> PL_BitString (true , n)
    | _ -> error ~loc:(Location.make $startpos $endpos) PE_Unknown
  }

(* -------------------------------------------------------------------- *)
%inline term:
| t=loc(term_r) { t }

(* -------------------------------------------------------------------- *)
%inline literal:
| l=loc(literal_r) { l }

(* ==================================================================== *)
%inline empty:
| (* empty *) { () }

(* -------------------------------------------------------------------- *)
%inline parens(X):
| LPAREN x=X RPAREN { x }

(* -------------------------------------------------------------------- *)
%inline braces(X):
| LBRACE x=X RBRACE { x }

(* -------------------------------------------------------------------- *)
%inline plist0(X, S):
| xs=separated_list(S, X) { xs }

(* -------------------------------------------------------------------- *)
%inline tuple0(X):
| xs=parens(plist0(X, COMMA)) { xs }

(* -------------------------------------------------------------------- *)
%inline seq0(X):
| xs=plist0(X, empty) { xs }

(* -------------------------------------------------------------------- *)
%inline loc(X):
| x=X {
    { pldesc = x;
      plloc  = Location.make $startpos $endpos;
    }
  }
