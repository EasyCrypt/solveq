open Core
open Monalg
open Types
open GroebnerBasis

module Dependencies(R : Field)(S : Monalg.MonAlgebra with type ring = R.t and type mon = X.t)(P : Monalg.ProductAlgebra with type ringA = S.t and type ringB = S.t)
=
struct
  module C = Converter(R)(S)

  module GB = GroebnerBasis.ProdGB(R)(S)(P)
  module M = Map.Make(V)
      
  let get_dependencies (pols : S.t list) (detvars : Set.Make(V).t) (rndvars : Set.Make(V).t) =
    (*  All variables in the polynom must be detvars union rnd vars. All variables in rndvars must be greater than those in detvars, and those in detvars greater than the fresh variable starting in "f". For instance, all variables in rndvar could be prefixed with "z", all those in det with "x". *)
    let counter = ref (0) in
    let map = ref M.empty in
      let ps = List.fold_left (fun acc poly ->
          counter := !counter+1;
          let fresh_var =  S.form R.unit (X.ofvar (pvar_of_var (!counter)  ~pref:"f" ))  in
          map := M.add (pvar_of_var (!counter)  ~pref:"f" ) poly (!map);
          ((S.(-!) poly fresh_var,fresh_var))::acc
        ) [(S.unit, S.unit )] pols in
      let module Se = Set.Make(V) in
      let basis = GB.syz rndvars ps in
      (* we compute the GB of the subalgebra generated by <pols,1>, when allowing to multiply by detvars only.*)
      (* the extra variables fi allow to keep track of the computations, each element in the basis which does not depend on rnd vars is an element of the sizygy of <pols,detvars> *)
      let depvars = ref Se.empty in
      let res = List.filter (fun pol ->
          Se.is_empty (Se.inter rndvars (C.varset pol))
        )
          basis in
      let boundvarpol = List.fold_left (fun acc pol -> Se.union (C.varset pol) acc) Se.empty res in
      let boundvar = Se.inter boundvarpol detvars in
    let boundpol,unboundpol = M.partition (fun varpol pol -> Se.mem varpol boundvarpol) (!map) in
    let tolist = fun x -> List.map (fun (u,v) -> v) (M.bindings x) in
    Se.to_list boundvar, tolist boundpol, tolist unboundpol
end


