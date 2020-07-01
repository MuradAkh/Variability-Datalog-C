%token <string> ATOMIC
%token EQUALS
%token RIGHT_PAREN
%token ASSIGN_PAREN
%token CAST_PAREN
%token ARRAY_PAREN
%token POINTER_DEREF_PAREN
%token ATOMIC_NAMED_DECL_PAREN
%token DECL_ID_LIST_PAREN
%token POINTER_PAREN
%token ID_PAREN
%token OPT_PAREN
%token COMMA
%token OTHER_PAREN
%token INIT_PAREN
%token INITD_PAREN
%token POSTFIX_PAREN
%token POINTER_POSTFIX_PAREN
%token DEF_PAREN
%token CHOICE_PAREN
%token DEF_AND
%token DEF_AND_NOT
%token DEF_OR_NOT
%token DEF_OR
%token DEF_NOT
%token OPT_OR
%token OPT_AND
%token EOF

%start <TypechefTypes.c_ast option> prog
%%

prog:
    | EOF {None}
    | v = value {Some v}
    ;

/* ident:
    | p = pointer {TypechefTypes.IdAst(fst p, snd p)}  */


other:
    | ast = value; COMMA; rest = other {ast :: rest}
    | ast = value {[ast]}


ors: 
    | atom = ATOMIC; RIGHT_PAREN DEF_OR; rest = ors {(TypechefTypes.AtomV atom) :: rest}
    | atom = ATOMIC; RIGHT_PAREN DEF_OR_NOT; rest = ors_n {(TypechefTypes.AtomV atom) :: rest}
    | atom = ATOMIC; RIGHT_PAREN {[(TypechefTypes.AtomV atom)]}

ands_n: 
    | atom = ATOMIC; RIGHT_PAREN DEF_AND; rest = ands {TypechefTypes.NotV((TypechefTypes.AtomV atom)) :: rest}
    | atom = ATOMIC; RIGHT_PAREN DEF_AND_NOT; rest = ands_n {TypechefTypes.NotV((TypechefTypes.AtomV atom)) :: rest}
    | atom = ATOMIC; RIGHT_PAREN {[TypechefTypes.NotV((TypechefTypes.AtomV atom))]}

ors_n: 
    | atom = ATOMIC; RIGHT_PAREN DEF_OR; rest = ors {TypechefTypes.NotV((TypechefTypes.AtomV atom)) :: rest}
    | atom = ATOMIC; RIGHT_PAREN DEF_OR_NOT; rest = ors_n {TypechefTypes.NotV((TypechefTypes.AtomV atom)) :: rest}
    | atom = ATOMIC; RIGHT_PAREN {[TypechefTypes.NotV((TypechefTypes.AtomV atom))]}

ands: 
    | atom = ATOMIC; RIGHT_PAREN DEF_AND; rest = ands {(TypechefTypes.AtomV atom) :: rest}
    | atom = ATOMIC; RIGHT_PAREN DEF_AND_NOT; rest = ands_n {(TypechefTypes.AtomV atom) :: rest}
    | atom = ATOMIC; RIGHT_PAREN {[(TypechefTypes.AtomV atom)]}

ands_opt:
    | o = opt; RIGHT_PAREN OPT_AND; rest = ands_opt {o :: rest}
    | o = opt; RIGHT_PAREN {[o]}

ors_opt:
    | o = opt; RIGHT_PAREN OPT_OR; rest = ors_opt {o :: rest}
    | o = opt; RIGHT_PAREN {[o]}

opt: 
    | OTHER_PAREN; v = opt; RIGHT_PAREN {v} 
    | OTHER_PAREN; v = opt; RIGHT_PAREN; OPT_AND; rest = ands_opt {TypechefTypes.AndV(v :: rest)} 
    | OTHER_PAREN; v = opt; RIGHT_PAREN; OPT_OR; rest = ors_opt {TypechefTypes.OrV(v :: rest)} 
    | DEF_PAREN; atom = ATOMIC; RIGHT_PAREN {TypechefTypes.AtomV atom}
    | _ = ATOMIC {TypechefTypes.NoVar ()}
    | DEF_NOT; atom = ATOMIC; RIGHT_PAREN {TypechefTypes.NotV (TypechefTypes.AtomV atom)}

    | DEF_PAREN; atom = ATOMIC; RIGHT_PAREN; DEF_OR; rest = ors;  
        {TypechefTypes.OrV ((TypechefTypes.AtomV atom) ::rest)}
    | DEF_PAREN; atom = ATOMIC; RIGHT_PAREN; OPT_OR; rest = ors_opt;  
        {TypechefTypes.OrV ((TypechefTypes.AtomV atom) ::rest)}
    | DEF_NOT; atom = ATOMIC; RIGHT_PAREN; DEF_OR_NOT; rest = ors_n;  
        {TypechefTypes.OrV ((TypechefTypes.NotV(TypechefTypes.AtomV atom)) ::rest)}
    | DEF_PAREN; atom = ATOMIC; RIGHT_PAREN; DEF_AND; rest = ands;  
        {TypechefTypes.AndV ((TypechefTypes.AtomV atom)::rest)}
    | DEF_PAREN; atom = ATOMIC; RIGHT_PAREN; OPT_AND; rest = ands_opt;  
        {TypechefTypes.AndV ((TypechefTypes.AtomV atom)::rest)}
    | DEF_NOT; atom = ATOMIC; RIGHT_PAREN; DEF_AND_NOT; rest = ands_n;  
        {TypechefTypes.AndV ((TypechefTypes.NotV(TypechefTypes.AtomV atom))::rest)}

    | DEF_NOT; atom = ATOMIC; RIGHT_PAREN; DEF_OR; rest = ors;  
        {TypechefTypes.OrV ((TypechefTypes.NotV(TypechefTypes.AtomV atom)) ::rest)}
    | DEF_NOT; atom = ATOMIC; RIGHT_PAREN; OPT_OR; rest = ors_opt;  
        {TypechefTypes.OrV ((TypechefTypes.NotV(TypechefTypes.AtomV atom)) ::rest)}
    | DEF_PAREN; atom = ATOMIC; RIGHT_PAREN; DEF_OR_NOT; rest = ors_n;  
        {TypechefTypes.OrV ((TypechefTypes.AtomV atom) ::rest)}
    | DEF_NOT; atom = ATOMIC; RIGHT_PAREN; DEF_AND; rest = ands;  
        {TypechefTypes.AndV ((TypechefTypes.NotV(TypechefTypes.AtomV atom))::rest)}
    | DEF_NOT; atom = ATOMIC; RIGHT_PAREN; OPT_AND; rest = ands_opt;  
        {TypechefTypes.AndV ((TypechefTypes.NotV(TypechefTypes.AtomV atom))::rest)}
    | DEF_PAREN; atom = ATOMIC; RIGHT_PAREN; DEF_AND_NOT; rest = ands_n;  
        {TypechefTypes.AndV ((TypechefTypes.AtomV atom)::rest)}


    

value:
    /* | ASSIGN_PAREN; id = ident; COMMA; EQUALS; COMMA; rest = value; RIGHT_PAREN {TypechefTypes.AssignAst(id, rest)} */
    | ASSIGN_PAREN; o = value; COMMA; EQUALS; COMMA; rest = value; RIGHT_PAREN {TypechefTypes.AssignExprAst(o, rest)}
    | POSTFIX_PAREN; f = value; COMMA; s = value; RIGHT_PAREN {TypechefTypes.PostfixAst(f, s)}
    | ATOMIC_NAMED_DECL_PAREN; f = value; COMMA; s = value; COMMA; t = value; RIGHT_PAREN {TypechefTypes.AtomicNamedDecl(f, s, t)}
    | POINTER_POSTFIX_PAREN; f = value; COMMA; s = value; RIGHT_PAREN {TypechefTypes.PointerPostfixAst(f, s)}
    | ID_PAREN; atom = ATOMIC; RIGHT_PAREN {TypechefTypes.LoadAst(TypechefTypes.IdAst(atom, 0))} 
    | OTHER_PAREN; o = other; RIGHT_PAREN {TypechefTypes.OtherAst o} 
    | INIT_PAREN; o = other; RIGHT_PAREN {TypechefTypes.InitAst o} 
    /* | OPT_PAREN; OTHER_PAREN; _ = opt; RIGHT_PAREN; COMMA; v = value RIGHT_PAREN {v}  */
    | OPT_PAREN; _ = opt; COMMA; v = value RIGHT_PAREN {v} 
    | CHOICE_PAREN; _ = opt; COMMA; v = other RIGHT_PAREN {TypechefTypes.OtherAst(v)} 
    | INITD_PAREN; o = other; RIGHT_PAREN {TypechefTypes.InitDeclAst o} 
    | DECL_ID_LIST_PAREN; o = other; RIGHT_PAREN {TypechefTypes.DeclIdList o} 
    | POINTER_DEREF_PAREN; o = value; RIGHT_PAREN {TypechefTypes.PointerDerefAst o} 
    | CAST_PAREN; o = other; RIGHT_PAREN {TypechefTypes.CastAst o} 
    | ARRAY_PAREN; o = value; RIGHT_PAREN {TypechefTypes.ArrayAst o} 
    | POINTER_PAREN; o = value; RIGHT_PAREN {TypechefTypes.PointerAst o} 
    | OTHER_PAREN; RIGHT_PAREN {TypechefTypes.OtherAst []} 
    | atom = ATOMIC {TypechefTypes.AtomicAst atom}


