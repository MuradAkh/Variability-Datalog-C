%token <string> ATOMIC
%token EQUALS
%token RIGHT_PAREN
%token ASSIGN_PAREN
%token ID_PAREN
%token COMMA
%token OTHER_PAREN
%token EOF

%start <TypechefTypes.c_ast option> prog
%%

prog:
    | EOF {None}
    | v = value {Some v}
    ;

ident:
    | ID_PAREN; atom = ATOMIC; RIGHT_PAREN {TypechefTypes.IdAst atom} 



other:
    | ast = value; COMMA; rest = other {ast :: rest}
    | ast = value {[ast]}


value:
    | ASSIGN_PAREN; id = ident; COMMA; EQUALS; COMMA; rest = value; RIGHT_PAREN {TypechefTypes.AssignAst(id, rest)}
    | ID_PAREN; atom = ATOMIC; RIGHT_PAREN {TypechefTypes.LoadAst(TypechefTypes.IdAst(atom))} 
    | OTHER_PAREN; o = other; RIGHT_PAREN {TypechefTypes.OtherAst o} 
    | OTHER_PAREN; RIGHT_PAREN {TypechefTypes.OtherAst []} 
    | atom = ATOMIC {TypechefTypes.AtomicAst atom}


