{
  open AstParser
  exception SyntaxError of string
}

let comma = "," 
let equals = "=" | "+=" | "-=" | "*=" | "|=" | "&=" | "<<=" | ">>=" | "/="
let assign_paren = "AssignExpr("
let initd_paren = "InitDeclaratorI("
let init_paren = "Initializer("
let id_paren = "Id("
let other_paren =  ['!' 'a'-'z' 'A'-'Z' '_'] ['~' 'a'-'z' 'A'-'Z' '0'-'9' '_']* "(" | "("
let white = [' ' '\t']+
let atomic = 
['|' '/' '&' '~' '0'-'9' 'a'-'z' 'A'-'Z' '_' '+' '<' '>' '"' '-' '<' '>' '*' '.' '\\' '\'' '=' '^' '!' '?' '@' ] 
['~' ':' '$' '^' '?' '@' ' ''*' '/''!' '.' '&' '=' '|' '\\' '\'' '+' '=' '-' '<' '>' 'a'-'z' 'A'-'Z' '0'-'9' '_' '"' '~']*
let string = '"'['%' '0'-'9' 'a'-'z' 'A'-'Z' '_' '+' '<' '>'  '-' '<' '>' ' ' '\\' ]*'"'

rule read = 
  parse 
    | string {ATOMIC (Lexing.lexeme lexbuf)}
    | white { read lexbuf }
    | assign_paren { ASSIGN_PAREN }
    | initd_paren { INITD_PAREN }
    | "CastExpr(" { CAST_PAREN }
    | "PostfixExpr(" {POSTFIX_PAREN}
    | "PointerPostfixSuffix(" {POINTER_POSTFIX_PAREN}
    | "PointerDerefExpr(" { POINTER_PAREN }
    | "ArrayAccess(" { ARRAY_PAREN }
    | id_paren { ID_PAREN }
    | init_paren { INIT_PAREN } 
    | other_paren {OTHER_PAREN }
    | comma { COMMA }
    | equals { EQUALS }
    | ')' {RIGHT_PAREN}
    | "->" {ATOMIC("->")}
    | atomic {ATOMIC (Lexing.lexeme lexbuf)}
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof {EOF}