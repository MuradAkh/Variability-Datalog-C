open Base
open Typechef
open TypechefTypes
open Datalog

let container_split : string = "___"

(* DANGER ZONE - MUTABLE VARIABLE *)
let _ID_MALLOC : int ref = ref 0
let next_id_mutable _: string =
  _ID_MALLOC := !_ID_MALLOC + 1;
   Int.to_string !_ID_MALLOC
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
  | PointerAst(ast) -> f ast
  | OptAst(_, ast) -> f ast
  | DeclIdList(asts) -> List.map ~f:f asts |> List.concat
  | AtomicNamedDecl(a1, a2, a3) -> List.map ~f:f [a1; a2; a3] |> List.concat
  | NAryExpr(a1, a2) -> List.map ~f:f [a1; a2] |> List.concat
  | NArySubExpr(a1, a2) -> List.map ~f:f [a1; a2] |> List.concat

let do_transform_children f = function
  | OtherAst(asts) -> OtherAst(List.map ~f:f asts)
  | AssignTast(t, g) -> AssignTast(t, f g)
  | InitAst(asts) -> InitAst(List.map ~f:f asts)
  | InitDeclAst(asts) -> InitDeclAst(List.map ~f:f asts)
  | CastAst(asts) -> CastAst(List.map ~f:f asts)
  | DeclIdList(asts) -> DeclIdList(List.map ~f:f asts)
  | MallocTast(ast) -> MallocTast(f ast)
  | AssignExprAst(a1, a2) -> AssignExprAst(f a1, f a2)
  | AtomicNamedDecl(a1, a2, a3) -> AtomicNamedDecl(f a1, f a2, f a3)
  | PointerDerefAst(ast) -> PointerDerefAst(f ast)
  | PointerPostfixAst(a1, a2) -> PointerPostfixAst(f a1, f a2)
  | PostfixAst(a1, a2) -> PostfixAst(f a1, f a2)
  | OptAst(v, ast) -> OptAst(v, f ast)
  | ArrayAst(a1) -> ArrayAst(f a1)
  | PointerAst(a) -> PointerAst(f a)
  | NAryExpr(a1, a2) ->  NAryExpr(f a1, f a2)
  | NArySubExpr(a1, a2) ->  NArySubExpr(f a1, f a2)
  | LoadAst(a) -> LoadAst(a)
  | AtomicAst(a) -> AtomicAst(a)

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
  | PostfixAst(a, ArrayAst(_)) -> a
  | ast -> do_transform_children transform_array_access ast
  
let rec transform_mallocs ast = match ast with 
  | PostfixAst(LoadAst(IdAst(x, _)), _) when String.is_substring ~substring:"alloc" x -> 
    (* Logger.sLog @@ "transformed malloc: "  ^ (Sexp.to_string @@ sexp_of_c_ast ast); *)
    MallocTast(ast)
  | MallocTast(_) -> Logger.sLog @@ "malloc slef tr: "  ^ (Sexp.to_string @@ sexp_of_c_ast ast); ast
  | _ast -> do_transform_children transform_mallocs _ast

let rec transform_pointer_arithmetic ast = match ast with 
  | NAryExpr(id, OtherAst([OptAst(_, NArySubExpr(AtomicAst(op), _))])) when String.equal "+" op || String.equal "-" op -> 
    id
  | _ast -> do_transform_children transform_pointer_arithmetic _ast
  
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
      |> transform_pointer_arithmetic
      |> ast_assigns_helper


let ast_returns input_ast = 
  let rec ast_returns_helper = function
    | AssignTast(store, rest) -> (
        match rest with 
          | MallocTast(_) -> []
          | PostfixAst(LoadAst(IdAst(x, _)), _) when not @@ String.is_substring ~substring:"alloc" x -> [(store, x)]
          | _ -> ast_returns_helper rest
    )
    | InitDeclAst([a; _; c]) -> (match c with
      | OtherAst([InitAst([_; MallocTast(_)])]) -> []
      | OtherAst([InitAst([_; PostfixAst(LoadAst(IdAst(x, _)), _)])]) when not @@ String.is_substring ~substring:"alloc" x -> [(ast_loads a |> List.hd_exn, x)]
      | _ -> []
    )
    | other -> do_find_children ast_returns_helper other
  in

  input_ast 
      |> transform_casts 
      |> transform_assigns
      |> transform_struct_access
      |> transform_array_access
      |> transform_pointers
      |> transform_mallocs
      |> ast_returns_helper


let ast_mallocs input_ast = 
  let rec ast_mallocs_helper = function
    | AssignTast(store, rest) -> (
        match rest with 
        (* DANGER ZONE - MUTABLE VARIABLE *)
          | MallocTast(_) -> [(store, next_id_mutable ())]
        (* END DANGER ZONE *)
          | _ -> ast_mallocs_helper rest
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
      |> transform_assigns
      |> transform_struct_access
      |> transform_array_access
      |> transform_mallocs
      |> transform_pointers
      |> ast_mallocs_helper


  
let rec ast_loads_stores = function 
  | LoadAst(id) -> [id]
  | AssignTast(store, rest) -> store :: ast_loads_stores rest
  | other -> do_find_children ast_loads_stores other


let rec ast_decls = function 
    | AtomicNamedDecl(_, LoadAst(id), _) -> [id]
    | other -> do_find_children ast_decls other


let ast_returns_pointer input_ast =
  let rec ast_has_decl_list = function
    | DeclIdList(_) -> [()]
    | other -> do_find_children ast_has_decl_list other
  in

  let rec ast_has_pointer = function
    | PointerAst(_) -> [()]
    | other -> do_find_children ast_has_pointer other
  in

  let rec do_find_children_wrapper ast (varstack : varE list) = 
    do_find_children (fun a -> ast_returns_pointer_helper varstack a) ast
  and ast_returns_pointer_helper (varstack: varE list)= function
    | OptAst(var, ast) -> 
      let new_stack = match var with NoVar(_) -> varstack | _ -> var :: varstack in 
      ast_returns_pointer_helper new_stack ast
    | AtomicNamedDecl(rtype, LoadAst(IdAst(id, _)), decl) -> 
      if 
        not @@ List.is_empty @@ ast_has_decl_list decl 
        &&
        not @@ List.is_empty @@ ast_has_pointer rtype 
      then 
        [(id, varstack)]
      else []
    | other -> do_find_children_wrapper other varstack
  in 
  ast_returns_pointer_helper [] input_ast


let ast_finder (n : node) f t= 
  let Node{nodeValue = valn; _} = n in 
  let ids, container = match valn with 
    | Statement {value = ast; container = c} -> f ast, c
    | Declaration {value = ast; container = c} -> f ast, c
    | _ -> [], ""
  in
  
  List.map ~f:(t container) ids 


let returns_pointer_of_ast ast = 
  let func_names  = ast_returns_pointer ast in 
  List.map ~f:(fun (id, varstack) -> (match varstack with 
    | [] -> ReturnsPointer{variability=None; variable=id}
    | _ -> ReturnsPointer{variability=Some(AndV(varstack)); variable=id})
  ) func_names

(*AST to daltalog fact tuples*)
let unary_transformer container = function 
| IdAst(id, p1) -> id ^ container_split ^ Int.to_string p1 ^ container

let binary_transformer container = function 
| (IdAst(id, p1), IdAst(id2, p2)) -> (id ^ container_split ^ Int.to_string p1 ^ container, id2 ^ container_split  ^ Int.to_string p2 ^ container)

let malloc_transformer container = function 
| (IdAst(id, p1), i) -> (id ^ container_split ^ Int.to_string p1 ^ container, i)

let node_stores (n : node) : string list = 
  ast_finder n ast_stores unary_transformer

let node_loads_stores (n : node) : string list = 
  ast_finder n ast_loads_stores unary_transformer

let node_subnodes_count (n: node) : int = 
  node_loads_stores n |> List.length

let node_loads (n : node) : string list = 
  ast_finder n ast_loads unary_transformer

let node_decls (n : node) : string list = 
  ast_finder n ast_decls unary_transformer

let node_assigns (n : node) : (string * string) list = 
  ast_finder n ast_assigns binary_transformer

let node_mallocs (n : node) : (string * string) list = 
  ast_finder n ast_mallocs malloc_transformer

let node_returns (n : node) : (string * string) list = 
  ast_finder n ast_returns malloc_transformer