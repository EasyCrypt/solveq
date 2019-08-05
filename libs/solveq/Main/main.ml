open Core
open Io
open Location
open Monalg
open Types
open Uniform
    
module Se=Set.Make(Var)

let sb_pp = SB.pp;;

let print = Format.printf 

let printSe se =
  List.iter (print " %a " Var.pp) (Se.to_list se)
  
let isunif rings detvars rndvars=
     print "*** Checking uniformity *** @.";
     print "deterministic variables:";
     printSe (detvars);
     print "@.";
     print "random variables:";
     printSe (rndvars);
     print "@.";
     print "programs :";
List.iter (fun r -> print " %a " Types.pp_ring r) rings;
     print "@.";
     if (Uniform.is_unif_ring rings detvars rndvars) then
       (print "Result: uniform.@. @.";
       true)
     else
       (print "Result: No subsets of the random variables turn the program into a bijection.@. @.";
        false)

let invert varlist rings =
  print "*** Compute inverse functions *** @.";
  print "function :";
  List.iter (fun r -> print " %a " Var.pp r) varlist;
  print "->";
  List.iter (fun r -> print " %a " Types.pp_ring r) rings;
  print "@.";
  match Inverter.compute_inv_ring_tuple varlist rings with
  | Some(inv) ->
    print "inverter :";
    List.iteri (fun i _ -> print " %a " Var.pp (Var.make_fresh (Var.of_int (i+1)))) inv;
    print "->";
    List.iter (fun r -> print " %a " Types.pp_ring r) inv;
    print "@. @.";
    true
  | None -> print "No inverse. @. @."; false

exception ExprNotSupported
exception UnknownOperator of Location.t
exception UnknownLiteral of Location.t
exception UnknownCommand of Location.t


let lit_to_ring (i : Syntax.pliteral) : ring =
  match Location.unloc i with
  |PL_Int j when j = Big_int.zero -> ZeroR
  |PL_Int j when j = Big_int.unit_big_int -> UnitR
  | _ -> raise (UnknownLiteral (Location.loc i))


let rec converter (p : Syntax.pterm) (detvars : Se.t) (rndvars: Se.t ref) : ring =
  match Location.unloc p with
  |PLit i -> lit_to_ring i
  |PVar v ->
    let var = Var.of_string (Location.unloc v) in
    if not (Se.mem var detvars) then rndvars := Se.add var (!rndvars);
    VarR(var)
  |PApp(symb,ts) ->
    match symb with
    | `BinOp(op) ->
      (match ts with
       | [t1;t2] ->
         (
           let r1= converter t1 detvars rndvars and r2 = converter t2 detvars rndvars in
           match (Location.unloc op) with
           |`Add -> AddR(r1,r2)
           |`Mul -> MultR(r1,r2)
           |_ -> raise (UnknownOperator (Location.loc op))
         )
       | _-> raise ExprNotSupported
      )
    | `Named _ ->  raise ExprNotSupported 
  |_ -> raise ExprNotSupported
       
let run (success:bool ref) filename =
  let program = File.with_file_in filename Io.parse_program in
  let test = Location.unloc (List.hd program) in
  let detvars = ref Se.empty in
  let rndvars = ref Se.empty in
  let run_command (command : Syntax.pentry) =
    let c = Location.unloc command in
    match c with
    | PVarDecl(l) -> let varlist = List.map (fun v -> Var.of_string (unloc v)) l in
      detvars := Se.union (!detvars) (Se.of_list varlist)          
    | PIsUnif(pterms) ->
      let rings = List.map (fun p -> converter p (!detvars) (rndvars) ) pterms in
      success := (isunif rings (!detvars) (!rndvars)) && !success
    |PInvert(pvars,pterms) ->
      let varlist = List.map (fun v -> Var.of_string (unloc v)) pvars in
      let rings = List.map (fun p -> converter p (!detvars) (rndvars) ) pterms in
      success := (invert varlist rings) && !success
    | _ -> raise (UnknownCommand (Location.loc command))
        

  in
  List.iter run_command program
  
let main =
  let success = ref true in
  Arg.parse [] (run success) "executes files given in arguments. \n";
  if !success then
    (print "**** All commands successful ****@.";
      exit 0)
  else
    (print "**** Some commands failed ****@.";
     exit 1)
