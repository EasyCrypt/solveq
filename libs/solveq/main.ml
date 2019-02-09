open Core
open Io
open Location


let check filename =
  let program = File.with_file_in filename Io.parse_program in
  let test = Location.unloc (List.hd program) in
  match test with
  |  PVarDecl(l) -> List.iter (fun i -> print_string (Location.unloc i)) l
  | _ -> ()


let main =
  Arg.parse [] check "executes files given in arguments. \n"
