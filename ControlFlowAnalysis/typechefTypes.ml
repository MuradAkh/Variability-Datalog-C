open Base 

type varE = 
    | AtomV of string 
    | AndV of varE list
    | NotV of varE
    | OrV of varE list
    | NoVar of unit
[@@deriving sexp]

type c_ast_id = IdAst of string [@@deriving sexp]


type c_ast = 
    | AssignAst of c_ast_id * c_ast
    | OtherAst of c_ast list 
    | AtomicAst of string
    | LoadAst of c_ast_id
    [@@deriving sexp]

