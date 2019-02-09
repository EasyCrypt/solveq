(* -------------------------------------------------------------------- *)
{
  open Core
  open Parser

  module L = Location

  (* ------------------------------------------------------------------ *)
  let lex_error lexbuf msg =
    let loc = L.of_lexbuf lexbuf in
    raise (Syntax.LexError (Some loc, PE_LexicalError msg))

  (* ------------------------------------------------------------------ *)
  let _keywords = [
    ("isunif", ISUNIF);
    ("var"  , VAR  );
  ]

  (* ------------------------------------------------------------------ *)
  let keywords =
    let table = Hashtbl.create 0 in
    List.iter (uncurry (Hashtbl.add table)) _keywords; table
}

let empty   = ""
let blank   = [' ' '\t' '\r']
let newline = '\n'
let upper   = ['A'-'Z']
let lower   = ['a'-'z']
let letter  = upper | lower
let digit   = ['0'-'9']
let uint    = digit+
let ichar   = (letter | digit | '_' | '\'')
let ident   = (letter | '_') ichar*

(* -------------------------------------------------------------------- *)
rule main = parse
  | newline       { Lexing.new_line lexbuf; main lexbuf }
  | blank+        { main lexbuf }
  | ident  as id  { try Hashtbl.find keywords id with Not_found -> IDENT id }
  | digit+ as num { INT (Big_int.big_int_of_string num) }
  
  | '+' { PLUS   }
  | '*' { STAR   }
  | '^' { HAT    }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ',' { COMMA }

  | eof { EOF   }

  | _ as c
      { lex_error lexbuf (Printf.sprintf "illegal character: `%c'" c) }
