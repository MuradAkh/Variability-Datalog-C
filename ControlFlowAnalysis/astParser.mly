%token <string> ATOMIC
%token EQUALS
%token RIGHT_PAREN
%token ASSIGN_PAREN
%token MALLOC_PAREN
%token CAST_PAREN
%token POINTER_PAREN
%token ID_PAREN
%token COMMA
%token OTHER_PAREN
%token INIT_PAREN
%token INITD_PAREN
%token EOF

%start <TypechefTypes.c_ast option> prog
%%

prog:
    | EOF {None}
    | v = value {Some v}
    ;

ident:
    | p = pointer {TypechefTypes.IdAst(fst p, snd p)} 

pointer:
    | ID_PAREN; atom = ATOMIC; RIGHT_PAREN {(atom, 0)} 
    | POINTER_PAREN; p = pointer; RIGHT_PAREN {(fst p, (snd p) + 1)} 


other:
    | ast = value; COMMA; rest = other {ast :: rest}
    | ast = value {[ast]}


value:
    | ASSIGN_PAREN; id = ident; COMMA; EQUALS; COMMA; rest = value; RIGHT_PAREN {TypechefTypes.AssignAst(id, rest)}
    | ID_PAREN; atom = ATOMIC; RIGHT_PAREN {TypechefTypes.LoadAst(TypechefTypes.IdAst(atom, 0))} 
    | OTHER_PAREN; o = other; RIGHT_PAREN {TypechefTypes.OtherAst o} 
    | INIT_PAREN; o = other; RIGHT_PAREN {TypechefTypes.InitAst o} 
    | INITD_PAREN; o = other; RIGHT_PAREN {TypechefTypes.InitDeclAst o} 
    | POINTER_PAREN; o = pointer; RIGHT_PAREN {TypechefTypes.LoadAst(TypechefTypes.IdAst(fst o, snd o))} 
    | CAST_PAREN; o = other; RIGHT_PAREN {TypechefTypes.CastAst o} 
    | MALLOC_PAREN; o = value; RIGHT_PAREN {TypechefTypes.MallocAst o} 
    | OTHER_PAREN; RIGHT_PAREN {TypechefTypes.OtherAst []} 
    | atom = ATOMIC {TypechefTypes.AtomicAst atom}


