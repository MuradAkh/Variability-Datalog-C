open Typechef
open TypechefTypes

let rec ast_stores = function 
  | AtomicAst(_) -> []
  | OtherAst(asts) -> List.map ast_stores asts |> List.concat
  | LoadAst(_) -> []
  | AssignAst(store, rest) -> store :: ast_stores rest

let rec ast_loads = function 
  | AtomicAst(_) -> []
  | OtherAst(asts) -> List.map ast_stores asts |> List.concat
  | LoadAst(id) -> [id]
  | AssignAst(_, rest) -> ast_stores rest
  
let ast_finder (n : node) f = 
  let Node{nodeValue = valn; _} = n in 
  let ids, container = match valn with 
    | Statement {value = ast; container = c} -> f ast, c
    | Declaration {value = ast; container = c} -> f ast, c
    | _ -> [], ""
  in
  
  List.map (function | IdAst(id) -> id ^ "$$$$" ^ container) ids 

let node_stores (n : node) : string list = 
  ast_finder n ast_stores


let node_loads (n : node) : string list = 
  ast_finder n ast_loads