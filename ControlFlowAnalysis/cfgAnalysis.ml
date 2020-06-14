open Typechef
open TypechefTypes
open Datalog
open Base
open GraphTools

let container_split : string = "___"

(* DANGER ZONE - MUTABLE VARIABLE *)
let _ID_MALLOC : int ref = ref 0
let next_id_mutable _: int = 
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
      if Base.Set.mem set n then set else(
      let updated = Set.add set n in
      List.fold ~f:dfs ~init:updated @@ nexts n)
   in
   let out = dfs visited source |> Set.to_list in
   out

    
let do_find_children f = function
  | OtherAst(asts) -> List.map ~f:f asts |> List.concat
  | MallocTast(ast) -> f ast
  | AssignTast(_, rest) -> f rest
  | InitDeclAst(asts) -> List.map ~f:f asts |> List.concat
  | InitAst(asts) -> List.map ~f:f asts |> List.concat
  | CastAst(asts) -> List.map ~f:f asts |> List.concat
  | AssignExprAst(a1, a2) -> List.map ~f:f [a1; a2] |> List.concat
  | PointerDerefAst(ast) -> f ast
  | PointerPostfixAst(a1, a2) -> List.map ~f:f [a1; a2] |> List.concat
  | PostfixAst(a1, a2) -> List.map ~f:f [a1; a2] |> List.concat
  | ArrayAst(a1) -> f a1
  | AtomicAst(_) -> []
  | LoadAst(_) -> []


let do_transform_children f = function
    | OtherAst(asts) -> OtherAst(List.map ~f:f asts)
    | AssignTast(t, g) -> AssignTast(t, f g)
    | InitAst(asts) -> InitAst(List.map ~f:f asts)
    | InitDeclAst(asts) -> InitDeclAst(List.map ~f:f asts)
    | CastAst(asts) -> CastAst(List.map ~f:f asts)
    | MallocTast(ast) -> MallocTast(f ast)
    | AssignExprAst(a1, a2) -> AssignExprAst(f a1, f a2)
    | PointerDerefAst(ast) -> PointerDerefAst(f ast)
    | PointerPostfixAst(a1, a2) -> PointerPostfixAst(f a1, f a2)
    | PostfixAst(a1, a2) -> PostfixAst(f a1, f a2)
    | ArrayAst(a1) -> ArrayAst(f a1)
    | ast -> ast

let rec transform_casts = function 
  | CastAst([_; ast]) -> ast
  | ast -> do_transform_children transform_casts ast

let rec transform_struct_access = function 
  | PostfixAst(
      LoadAst(IdAst(strct, depth)), 
      PointerPostfixAst(AtomicAst("->"), LoadAst(IdAst(field, _)))
    ) -> LoadAst(IdAst(strct ^ "->" ^ field, depth))
  | PostfixAst(
      LoadAst(IdAst(strct, depth)), 
      PointerPostfixAst(AtomicAst("."), LoadAst(IdAst(field, _)))
    ) -> LoadAst(IdAst(strct ^ "." ^ field, depth))
  | ast -> do_transform_children transform_struct_access ast

let rec transform_assigns = function 
  | AssignExprAst(LoadAst(id), rest) -> AssignTast(id, rest)
  | ast -> do_transform_children transform_assigns ast


let rec transform_pointers = function
  | PointerDerefAst(ast) -> 
    (match transform_pointers ast with
      | LoadAst(IdAst(str, i)) -> LoadAst(IdAst(str, i + 1))
      | _ -> ast)
  | ast -> do_transform_children transform_pointers ast

  let rec transform_array_access ast = match ast with 
    | OtherAst(a :: b :: asts) -> (match b with
        | ArrayAst(_) -> a
        | _ -> OtherAst(List.map ~f:transform_array_access (a :: b :: asts)))
    | ast -> do_transform_children transform_array_access ast
  

let rec ast_loads = function 
  | LoadAst(id) -> [id]
  | other -> do_find_children ast_loads other

let ast_stores input_ast =   
  let rec ast_stores_helper = function
    | AssignTast(store, rest) -> store :: ast_stores_helper rest
    | other -> do_find_children ast_stores_helper other
  in 

  input_ast 
    |> transform_assigns 
    |> ast_stores_helper

let  ast_assigns input_ast = 
   let  rec ast_assigns_helper = function 
      | InitDeclAst([a; _; c]) -> (match c with
        | OtherAst([InitAst([_; LoadAst(ld)])]) -> [(ast_loads a |> List.hd_exn, ld)]
        | _ -> []
      )
      | AssignTast(store, rest) -> (
          match rest with 
            | LoadAst(r) -> [(store, r)]
            | _ -> [])
      | other -> do_find_children ast_assigns_helper other
   in 

   input_ast 
      |> transform_casts 
      |> transform_struct_access 
      |> transform_pointers
      |> transform_array_access 
      |> transform_assigns
      |> ast_assigns_helper


let ast_mallocs input_ast = 
  let rec transform_mallocs ast = match ast with 
    | PostfixAst(LoadAst(IdAst(x, _)), _) when String.is_substring ~substring:"alloc" x -> 
      (* Logger.sLog @@ "transformed malloc: "  ^ (Sexp.to_string @@ sexp_of_c_ast ast); *)
      MallocTast(ast)
    | MallocTast(_) -> Logger.sLog @@ "malloc slef tr: "  ^ (Sexp.to_string @@ sexp_of_c_ast ast); ast
    | _ast -> do_transform_children transform_mallocs _ast
  in 

  let rec ast_mallocs_helper = function
    | AssignTast(store, rest) -> (
        match rest with 
        (* DANGER ZONE - MUTABLE VARIABLE *)
          | MallocTast(_) -> [(store, next_id_mutable ())]
          | _ -> ast_mallocs_helper rest
        (* END DANGER ZONE *)
    )
    | InitDeclAst([a; _; c]) -> (match c with
      (* DANGER ZONE - MUTABLE VARIABLE *)
      | OtherAst([InitAst([_; MallocTast(_)])]) -> [(ast_loads a |> List.hd_exn, next_id_mutable ())]
      | _ -> []
      (* END DANGER ZONE *)
    )
    | other -> do_find_children ast_mallocs_helper other
  in

  input_ast 
      |> transform_casts 
      |> transform_pointers
      |> transform_assigns
      |> transform_mallocs 
      |> transform_struct_access
      |> transform_array_access
      |> ast_mallocs_helper
    

  
let rec ast_loads_stores = function 
  | LoadAst(id) -> [id]
  | AssignTast(store, rest) -> store :: ast_loads_stores rest
  | other -> do_find_children ast_loads_stores other



let ast_finder (n : node) f t= 
  let Node{nodeValue = valn; _} = n in 
  let ids, container = match valn with 
    | Statement {value = ast; container = c} -> f ast, c
    | Declaration {value = ast; container = c} -> f ast, c
    | _ -> [], ""
  in
  
  List.map ~f:(t container) ids 


(*AST to daltalog fact tuples*)
let unary_transformer container = function 
| IdAst(id, p1) -> id ^ container_split ^ Int.to_string p1 ^ container

let binary_transformer container = function 
| (IdAst(id, p1), IdAst(id2, p2)) -> (id ^ container_split ^ Int.to_string p1 ^ container, id2 ^ container_split  ^ Int.to_string p2 ^ container)

let malloc_transformer container = function 
| (IdAst(id, p1), i) -> (id ^ container_split ^ Int.to_string p1 ^ container, Int.to_string i)

let node_stores (n : node) : string list = 
  ast_finder n ast_stores unary_transformer

let node_loads_stores (n : node) : string list = 
  ast_finder n ast_loads_stores unary_transformer

let node_subnodes_count (n: node) : int = 
  node_loads_stores n |> List.length

let node_loads (n : node) : string list = 
  ast_finder n ast_loads unary_transformer

let node_assigns (n : node) : (string * string) list = 
  ast_finder n ast_assigns binary_transformer

let node_mallocs (n : node) : (string * string) list = 
  ast_finder n ast_mallocs malloc_transformer


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

let pointer_analysis_of_func (func: node) : datalog_fact list = 
  reachable succs_node func
  |> fun a -> List.map ~f:mallocs_of_node a @ List.map ~f:assigns_of_node a
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
      | Node{nodeID=id; varNode=var; _} ->
      NodeCycle{variability=Some(var); nodeId=id; cycleId= Int.to_string !counter} :: acc
    in

    List.fold ~init:[] ~f:make_nodecycles cycle 
    @ Cycle {
      variability=Some(AndV(List.map ~f:(function Node{varNode=v; _} -> v) cycle)); 
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
  List.map ~f:(fun (a, b) -> Dominator {variability=None; doms=(id_of_node a); domed=(id_of_node b)}) doms

let dominance (func : node) : datalog_fact list =
  generic_dom func succs_node

let post_dominance (func : node) : datalog_fact list =
  (* TODO: last node??*)
  generic_dom func preds_node ~g_pred:GI.succ ~g_succ:GI.pred