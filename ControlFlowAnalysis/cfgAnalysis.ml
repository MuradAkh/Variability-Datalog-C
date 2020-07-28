open Typechef
open TypechefTypes
open Datalog
open Base
open GraphTools
open AstAnalysis

let var_of_two = function
      | (NoVar (), NoVar ()) -> None 
      | (v, NoVar ()) -> Some(v)
      | (NoVar (), v) -> Some(v)
      | (v1, v2) -> Some(AndV([v1; v2]))
    

let succs_node (n : node) = 
  let Node {succs = edges; _} = n in
  List.map ~f:(function | Edge {b = s; _} -> s ) edges 


let preds_node (n : node) = 
  let Node {preds = edges; _} = n in
  List.map ~f:(function | Edge {a = p; _} -> p ) edges 


let assigns_of_node (target : node): datalog_fact list =
  let Node {varNode = varb; _} = target in
  let assigns = node_assigns target in 

  let make_assign (tovar, fromvar) = 
    Assign {tovar = tovar; variability = Some(varb); fromvar=fromvar} 
  in
  
  List.map ~f:make_assign assigns


let mallocs_of_node (target : node): datalog_fact list =
  let Node {varNode = varb; _} = target in
  let mallocs = node_mallocs target in 
  let make_malloc (var, heapid) = 
    PointsTo {variable = var; variability = Some(varb); heap=heapid} 
  in
  
  List.map ~f:make_malloc mallocs

let returns_of_node (target : node): datalog_fact list =
  let Node {varNode = varb; _} = target in
  let mallocs = node_returns target in 
  let make_return (var, heapid) = 
    AssignedReturn {variable = var; variability = Some(varb); heap=heapid} 
  in
  
  List.map ~f:make_return mallocs


let store_loads_of_node (target : node): datalog_fact list =
  let id = id_of_node target in
  let Node {varNode = varb; succs=succs; preds=preds; _} = target in
  let loads = node_loads target in 
  let stores = node_stores target in 

  let make_store (fact : string) (id2: int) = 
    Store {variable = fact; variability = Some(varb); node= id ^ ":" ^ (Int.to_string id2)} 
  in
  
  let make_load (fact : string) (id2: int) = 
    Load {variable = fact; variability = Some(varb); node= id ^ ":" ^ (Int.to_string id2)}
  in

  let make_inner_edge (id2: int) = 
    Edge {
      variability = Some(varb); 
      nodeAId= id ^ ":" ^ (Int.to_string id2); 
      nodeBId= id ^ ":" ^ (Int.to_string @@ id2 + 1)
      }
  in

  let make_outer_edge_succs (e: Typechef.edge) = 
    Edge {
      variability = Some(varb); 
      nodeAId= id;
      nodeBId= (match e with Typechef.Edge{b=b; _} -> id_of_node b)
    }
  in

  let make_outer_edge_preds (e: Typechef.edge) = 
    Edge {
      variability = Some(varb); 
      nodeAId= (match e with Typechef.Edge{a=a; _} -> id_of_node a);
      nodeBId= id;
    }
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

  let (sl_edge_facts, _) = 
    List.fold_left 
      ~f:(fun (l, n) _ -> ((make_inner_edge n) :: l, n - 1)) 
      ~init:(sl_facts, (node_subnodes_count target) - 1) 
      (stores @ loads)
  in


  (List.map ~f:make_outer_edge_preds preds) @
  (List.map ~f:make_outer_edge_succs succs) @
  sl_edge_facts


let store_loads_of_func (func: node) : datalog_fact list = 
  reachable succs_node func 
  |> List.map ~f:store_loads_of_node
  |> List.concat

let edges_of_func (func: node) : datalog_fact list = 
  reachable succs_node func 
  |> List.map ~f:(function | Node{succs=ed; _} -> ed)
  |> List.concat 
  |> List.map 
    ~f:(function | Edge{a=a; b=b; varEdge=var; _} 
      -> Datalog.Edge{variability=Some(var); nodeAId=id_of_node a; nodeBId= id_of_node b}
    )


let pointer_analysis_of_func (func: node) : datalog_fact list = 
  reachable succs_node func
  |> fun a -> 
    List.map ~f:mallocs_of_node a @ 
    List.map ~f:returns_of_node a @
    List.map ~f:assigns_of_node a
  |> List.concat

let node_cross ((a, b) : node * node) = 
  ignore a;
  ignore b;
  ()

let cycles (func: node) : datalog_fact list = 
  let nodes = reachable succs_node func in
  let graph = to_impertive @@ make_graph nodes in 

  (* DANGER - MUTABLE VAR *)
  let counter : int ref = ref 0 in 

  let make_all_facts (facts: datalog_fact list) (cycle: node list) = 
    (* DANGER - MUTATION *)
    counter := !counter + 1;
    (* END DANGER - MUTATION *)

    let make_nodecycles acc = function 
      | (Node{nodeID=id1; varNode=var1; _}, Node{nodeID=id2; varNode=var2; _}) ->
      NodeCycle{variability=var_of_two (var1, var2); nodeAId=id1; nodeBId=id2; cycleId= Int.to_string !counter} :: acc
    in

    let shifted = List.tl_exn cycle @ [List.hd_exn cycle] in 
    
    let cycle_vars = 
      match List.map ~f:(function Node{varNode=v; _} -> v) cycle |> List.filter ~f:(function | NoVar () -> false | _ -> true) with 
      | [] -> None 
      | [a] -> Some(a)
      | a -> Some(AndV(a))
    in
    (List.fold ~init:[] ~f:make_nodecycles @@ List.zip_exn cycle shifted)  
    @ Cycle {
      variability=cycle_vars; 
      id= Int.to_string !counter
    }
    :: facts
  in

  Cycles.find_all_cycles_johnson graph
  |> List.fold ~init:[] ~f:make_all_facts  


let generic_dom ?g_pred ?g_succ (goal: node) (nexts : node -> node list) : datalog_fact list = 
  let nodes = reachable nexts goal in
  let graph = make_graph nodes in 
  let doms = dominaors graph goal nodes ?g_pred ?g_succ in 
  List.map ~f:(fun (a, b) -> 
      Dominator {variability=var_of_two (var_of_node a, var_of_node b); doms=(id_of_node a); domed=(id_of_node b)}
  ) doms

let dominance (func : node) : datalog_fact list =
  generic_dom func succs_node

let post_dominance (func : node) : datalog_fact list =
  (* TODO: last node??*)
  generic_dom func preds_node ~g_pred:GI.succ ~g_succ:GI.pred