open Typechef
open TypechefTypes
open Datalog


let rec ast_stores = function 
  | AtomicAst(_) -> []
  | OtherAst(asts) -> List.map ast_stores asts |> List.concat
  | LoadAst(_) -> []
  | AssignAst(store, rest) -> store :: ast_stores rest

let rec ast_loads = function 
  | AtomicAst(_) -> []
  | OtherAst(asts) -> List.map ast_loads asts |> List.concat
  | LoadAst(id) -> [id]
  | AssignAst(_, rest) -> ast_loads rest
  
let rec ast_loads_stores = function 
  | AtomicAst(_) -> []
  | OtherAst(asts) -> List.map ast_loads_stores asts |> List.concat
  | LoadAst(id) -> [id]
  | AssignAst(store, rest) -> store :: ast_loads_stores rest


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

let node_loads_stores (n : node) : string list = 
  ast_finder n ast_loads_stores

let node_subnodes_count (n: node) : int = 
  node_loads_stores n |> List.length

let node_loads (n : node) : string list = 
  ast_finder n ast_loads


let succs_node (n : node) = 
  let Node {succs = edges; _} = n in
  List.map (function | Edge {a = s; _} -> s ) edges 


let preds_node (n : node) = 
  let Node {preds = edges; _} = n in
  List.map (function | Edge {b = p; _} -> p ) edges 

let store_load_facts (func : node): datalog_fact list =
  ignore func;
  []


let generic_dom (goal: node) (nexts : node -> node list) : datalog_fact list = 

  goal |> nexts |> ignore;
  []

let dominance (func : node) : datalog_fact list =
  generic_dom func succs_node

let post_dominance (func : node) : datalog_fact list =
  (* TODO: last node??*)
  generic_dom func preds_node


