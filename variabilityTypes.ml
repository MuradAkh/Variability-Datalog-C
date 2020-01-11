open Base 

type varE = 
    | AtomV of string 
    | AndV of varE list
    | NotV of varE
    | OrV of varE list
    | NoVar of unit
[@@deriving sexp]