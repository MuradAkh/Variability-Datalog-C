open Typechef
open TypechefTypes
open Datalog
open Base
open GraphTools


let rec ast_stores = function 
  | AtomicAst(_) -> []
  | OtherAst(asts) -> List.map ~f:ast_stores asts |> List.concat
  | LoadAst(_) -> []
  | AssignAst(store, rest) -> store :: ast_stores rest

let rec ast_loads = function 
  | AtomicAst(_) -> []
  | OtherAst(asts) -> List.map ~f:ast_loads asts |> List.concat
  | LoadAst(id) -> [id]
  | AssignAst(_, rest) -> ast_loads rest
  
let rec ast_loads_stores = function 
  | AtomicAst(_) -> []
  | OtherAst(asts) -> List.map ~f:ast_loads_stores asts |> List.concat
  | LoadAst(id) -> [id]
  | AssignAst(store, rest) -> store :: ast_loads_stores rest


let ast_finder (n : node) f = 
  let Node{nodeValue = valn; _} = n in 
  let ids, container = match valn with 
    | Statement {value = ast; container = c} -> f ast, c
    | Declaration {value = ast; container = c} -> f ast, c
    | _ -> [], ""
  in
  
  List.map ~f:(function | IdAst(id) -> id ^ "$$$$" ^ container) ids 

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
  List.map ~f:(function | Edge {b = s; _} -> s ) edges 


let preds_node (n : node) = 
  let Node {preds = edges; _} = n in
  List.map ~f:(function | Edge {a = p; _} -> p ) edges 

let store_load_facts (func : node): datalog_fact list =
  ignore func;
  []


module MNode = struct
    module T = 
       struct  
          type t = node [@@deriving sexp_of, compare]  
       end
    include T
    include Base.Comparable.Make(T)
end

let reachable (nexts: node -> node list) (source : node) : node list  = 
   let visited = Base.Set.empty (module MNode) in

   let rec dfs set n = 
      if Base.Set.mem set n then set else
      let updated = Set.add visited n in
      List.fold ~f:dfs ~init:updated @@ nexts n
   in

   dfs visited source |> Set.to_list

let generic_dom ?g_pred ?g_succ (goal: node) (nexts : node -> node list) : datalog_fact list = 
  let nodes = reachable nexts goal in
  let graph = make_graph nodes in 
  let doms = dominaors graph goal nodes ?g_pred ?g_succ in 
  List.map ~f:(fun (a, b) -> Dominator {variability=None; doms=(id_of_node a); domed=(id_of_node b)}) doms

let dominance (func : node) : datalog_fact list =
  generic_dom func succs_node

let post_dominance (func : node) : datalog_fact list =
  (* TODO: last node??*)
  generic_dom func preds_node ~g_pred:G.succ ~g_succ:G.pred