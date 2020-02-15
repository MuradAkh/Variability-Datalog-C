{
  open VarParser
  exception SyntaxError of string
}

let or = "||"
let not = "!"
let defined_paren = "definedEx("
let white = [' ' '\t']+
let atomic = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = 
  parse 
    | white { read lexbuf }
    | defined_paren { DEFINED_PAREN }
    | '(' {LEFT_PAREN}
    | ')' {RIGHT_PAREN}
    | or {OR}
    | "&&" {AND}
    | not {NOT}
    | atomic {ATOMIC (Lexing.lexeme lexbuf)}
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof {EOF}