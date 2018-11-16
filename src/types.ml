open Core

type var = int

type group = 
  | Zero
  | Opp of group
  | Add of group*group
  | Var of var

let rec compute_inv (v:var) (g:group) =
  match g with
  | Zero -> None
  | Opp g -> Opt.map (fun x -> Opp x) (compute_inv v g)
  | Var x -> if x=v then Some (Var v) else None
  | Add(g1,g2) ->
      match (compute_inv v g1,compute_inv v g2) with
      | None, None -> None
      | Some g, None -> Some (Add (g,Opp g2))
      | None, Some g -> Some (Add (Opp g1,g))
      | _,_ -> None
                           


