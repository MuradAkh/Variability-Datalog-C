{
  open AstParser
  exception SyntaxError of string
}

let comma = ","
let equals = "="
let assign_paren = "AssignExpr("
let id_paren = "Id("
let other_paren =  ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* "("
let white = [' ' '\t']+
let atomic = ['0'-'9' 'a'-'z' 'A'-'Z' '_' '+' '<' '>'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read = 
  parse 
    | white { read lexbuf }
    | assign_paren { ASSIGN_PAREN }
    | id_paren { ID_PAREN }
    | other_paren {OTHER_PAREN }
    | comma { COMMA }
    | equals { EQUALS }
    | ')' {RIGHT_PAREN}
    | atomic {ATOMIC (Lexing.lexeme lexbuf)}
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof {EOF}