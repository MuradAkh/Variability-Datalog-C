%token <string> ATOMIC
%token OR
%token AND
%token NOT
%token LEFT_PARENT
%token RIGHT_PAREN
%token DEFINED_PAREN
%token EOF

%start <varE option> prog
%%

prog:
    | EOF {None}
    | v = value {Some v}
    ;


value:
    | DEFINED_PAREN; atom = atomic_string; RIGHT_BRACE
    {AtomV atom}
    | NOT; not = not_val
    {NotV not}

