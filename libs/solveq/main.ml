open Core
open Io
open Location
open Types
    
module Se=Set.Make(Var)

let mon_pp = X.pp Var.pp;;
let bi_pp fmt bi = Format.pp_print_int fmt  (Big_int.int_of_big_int bi);;
let b_pp = B.pp bi_pp;;
let r_pp = R.pp bi_pp;;
let s_pp = S.pp mon_pp r_pp;;
let p_pp = P.pp s_pp s_pp;;
let sb_pp = SB.pp mon_pp b_pp;;

let pstring = IO.write_string stdout

let isunif rings detvars =
     IO.write_line stdout "*** Checking uniformity ***";
     IO.write_string stdout "deterministic variables :";
     Se.print IO.write_string stdout (detvars);
     pstring "\n program :"
(*     List.print (fun sout pterm -> let fmt = Format.formatter_of_output stdout in
                       
                     ) stdout l *)

exception ExprNotSupported

let lit_to_ring (i : Syntax.pliteral_r) : ring =
  match i with
  |PL_Int j when j = Big_int.zero -> ZeroR
  |PL_Int j when j = Big_int.unit_big_int -> UnitR
  | _ -> raise ExprNotSupported


let converter (p : Syntax.pterm_r) : ring =
  match p with
  |PLit i -> lit_to_ring (Location.unloc i)
  |PVar v -> Var.of_string (Location.unloc v)
  |_ -> raise ExprNotSupported
       
let run filename =
  let program = File.with_file_in filename Io.parse_program in
  let test = Location.unloc (List.hd program) in
  let detvars = ref Se.empty in
  let run_command (command : Syntax.pentry) =
    let command = Location.unloc command in
      match command with
        | PVarDecl(l) -> let varlist = List.map unloc l in
          detvars := Se.union (!detvars) (Se.of_list varlist)          
        | PIsUnif(l) -> let pterms = List.map Location.unloc l in
          let rings = List.map converter pterms in
          isunif rings (!detvars)
       

  in
  List.iter run_command program

let main =
  Arg.parse [] run "executes files given in arguments. \n"
