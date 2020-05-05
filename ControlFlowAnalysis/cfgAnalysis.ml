open Typechef
open TypechefTypes
open Datalog
open Base
open GraphTools

(* DANGER ZONE - MUTABLE VARIABLE *)
let _ID_MALLOC : int ref = ref 0
let next_id_mutable : int = 
  _ID_MALLOC := !_ID_MALLOC + 1;
  !_ID_MALLOC
(* END DANGER ZONE *)


module MNode = struct
    module T = 
       struct  
          type t = node [@@deriving sexp_of]  
          let compare a b = String.compare (id_of_node a) (id_of_node b)
       end
    include T
    include Base.Comparable.Make(T)
end



let reachable (nexts: node -> node list) (source : node) : node list  = 
   let visited = Set.empty (module MNode) in

   let rec dfs set n = 
      (* if Base.Set.mem set n then set else *)
      let updated = Set.add set n in
      List.fold ~f:dfs ~init:updated @@ nexts n
   in
   dfs visited source |> Set.to_list

let rec ast_stores = function 
  | AtomicAst(_) -> []
  | OtherAst(asts) -> List.map ~f:ast_stores asts |> List.concat
  | MallocAst(ast) -> ast_stores ast
  | LoadAst(_) -> []
  | AssignAst(store, rest) -> store :: ast_stores rest

let rec ast_assigns = function 
  | AtomicAst(_) -> []
  | OtherAst(asts) -> List.map ~f:ast_assigns asts |> List.concat
  | LoadAst(_) -> []
  | MallocAst(ast) -> ast_assigns ast
  | AssignAst(store, rest) -> 
      match rest with 
        | LoadAst(r) -> [(store, r)]
        | _ -> []

let rec ast_mallocs = function 
  | AtomicAst(_) -> []
  | OtherAst(asts) -> List.map ~f:ast_mallocs asts |> List.concat
  | LoadAst(_) -> []
  | MallocAst(ast) -> ast_mallocs ast
  | AssignAst(store, rest) -> 
      match rest with 
      (* DANGER ZONE - MUTABLE VARIABLE *)
        | MallocAst(_) -> [(store, next_id_mutable)]
        | _ -> []
      (* END DANGER ZONE *)

let rec ast_loads = function 
  | AtomicAst(_) -> []
  | OtherAst(asts) -> List.map ~f:ast_loads asts |> List.concat
  | LoadAst(id) -> [id]
  | MallocAst(ast) -> ast_loads ast
  | AssignAst(_, rest) -> ast_loads rest
  
let rec ast_loads_stores = function 
  | AtomicAst(_) -> []
  | OtherAst(asts) -> List.map ~f:ast_loads_stores asts |> List.concat
  | LoadAst(id) -> [id]
  | MallocAst(ast) -> ast_loads_stores ast
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

let store_loads_of_node (target : node): datalog_fact list =
  let id = id_of_node target in
  let Node {varNode = varb; _} = target in
  let loads = node_loads target in 
  let stores = node_stores target in 

  let make_store (fact : string) (id2: int) = 
    Store {variable = fact; variability = Some(varb); node= id ^ ":" ^ (Int.to_string id2)} 
  in
  
  let make_load (fact : string) (id2: int) = 
    Load {variable = fact; variability = Some(varb); node= id ^ ":" ^ (Int.to_string id2)}
  in

  
  let (load_facts, rem) = 
    List.fold_left 
      ~f:(fun (l, n) curr -> ((make_load curr n) :: l, n - 1)) 
      ~init:([], node_subnodes_count target) 
      loads 
  in

  let (sl_facts, _) = 
    List.fold_left 
      ~f:(fun (l, n) curr -> ((make_store curr n) :: l, n - 1)) 
      ~init:(load_facts, rem) 
      stores 
  in

 
  (* Generate Dominator relations for subnodes *)
  let rec make_doms = function 
    | 0 -> sl_facts
    | n -> 

    let rec make_dom = function 
      | 0 ->  make_doms @@ n -1
      | m -> 
        Dominator {variability=Some(varb); doms= id ^ ":" ^ Int.to_string n; domed=id ^ ":" ^ Int.to_string n} 
          ::
        (make_dom @@ m - 1)
    in

    make_dom @@ n - 1
  in


  make_doms @@ node_subnodes_count target


let store_loads_of_func (func: node) : datalog_fact list = 
  reachable succs_node func 
  |> List.map ~f:store_loads_of_node
  |> List.concat

let node_cross ((a, b) : node * node) = ()


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