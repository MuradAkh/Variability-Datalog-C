%token <string> ATOMIC
%token OR
%token NOT
%token AND
%token LEFT_PAREN
%token RIGHT_PAREN
%token DEFINED_PAREN
%token EOF

%start <TypechefTypes.varE option> prog
%%

prog:
    | EOF {None}
    | v = value {Some v}
    ;

ors: 
    | v = value; OR; rest = ors {v :: rest}
    | v = value {[v]}

ands: 
    | v = value; AND; rest = ands {v :: rest}
    | v = value {[v]}


value:
    | DEFINED_PAREN; atom = ATOMIC; RIGHT_PAREN {TypechefTypes.AtomV atom}
    | LEFT_PAREN; v = value; OR; rest = ors; RIGHT_PAREN {TypechefTypes.OrV (v::rest)}
    | LEFT_PAREN; v = value; AND; rest = ands; RIGHT_PAREN {TypechefTypes.AndV (v::rest)}
    | NOT; v = value; {TypechefTypes.NotV v}



