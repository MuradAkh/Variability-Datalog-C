open Base 

type varE = 
    | AtomV of string 
    | AndV of varE list
    | NotV of varE
    | OrV of varE list
    | NoVar of unit
[@@deriving sexp, compare]

type c_ast_id = IdAst of string * int [@@deriving sexp, compare]

let string_of_ast_id = function | IdAst(s, _) -> s


type c_ast = 
    | AssignAst of c_ast_id * c_ast
    | OtherAst of c_ast list 
    | AtomicAst of string
    | InitAst of c_ast list
    | InitDeclAst of c_ast list 
    | CastAst of c_ast list 
    | LoadAst of c_ast_id
    | MallocAst of c_ast
    | ArrayAst of c_ast
    [@@deriving sexp, compare]

