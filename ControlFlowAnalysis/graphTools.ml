open Typechef

(* Thank you lambdapower @ Stackoverflow !! *)
(* https://stackoverflow.com/questions/8999557/how-to-visualize-draw-automata-in-ocaml/9011334#9011334 *)

(* representation of a node -- must be hashable *)
module GNode = struct
   type t = node
   let compare a b = Base.String.compare (id_of_node a) (id_of_node b)
   let hash = fun (a:node) -> Base.Hashtbl.hash (id_of_node a)
   let equal a b = Base.String.equal (id_of_node a) (id_of_node b)
end

(* representation of an edge -- must be comparable *)
module GEdge = struct
   type t = Base.string
   let compare = Base.String.compare
   let equal = Base.String.equal
   let default = ""
end


(* a functional/persistent graph *)
module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(GNode)(GEdge)
module GI = Graph.Imperative.Digraph.ConcreteBidirectionalLabeled(GNode)(GEdge)
module Dfs = Graph.Traverse.Dfs(G)


module Dot = Graph.Graphviz.Dot(struct
   include G (* use the graph module from above *)
   let edge_attributes (_, e, _) = [`Label e; `Color 4711]
   let default_edge_attributes _ = []
   let get_subgraph _ = None
   let vertex_attributes _ = [`Shape `Box]
   let vertex_name = fun a -> a 
      |> (function | Node {nodeID = nv; _} -> nv)
      (* |> (function | Node {nodeValue = nv; _} -> nv)
      |> function 
         | Function {value=v; _} -> v
         | FunctionInline {value=v; _} -> v
         | Unparsed {value=v; _} -> v
         | Declaration {value=ast; _} -> sexp_of_c_ast ast |> Base.Sexp.to_string 
         | _ -> "" *)
   let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let make_graph (nodes: node list) : G.t = 

   (* let strings_pair_nodes ((a, b) : node * node) : string * string = 
      let Node {nodeID = n1; _} = a in 
      let Node {nodeID = n2; _} = b in 
      (n1, n2)
   in *)

   let initial = G.empty in 
   let edges : 'a list =  
               List.map (function | Node {succs = e; _} -> e) nodes 
               |> List.concat 
               |> List.map (function | Edge {a = p; b = s;  _} -> (p, s))
               (* |> List.map strings_pair_nodes *)
   in
   (* List.map (function | Node {nodeID = id; _} -> id) *)
   nodes
      |> List.fold_left (fun acc curr -> G.add_vertex acc curr) initial
      |> fun g -> List.fold_left (fun acc (a, b) -> G.add_edge acc a b) g edges 


let plot (graph : G.t) = 
   let file = Stdio.Out_channel.create "output_cfg.dot" in
   Dot.output_graph file graph

let to_impertive (pers: G.t) : GI.t = 
   let imp = GI.create ~size:(G.nb_vertex pers) () in 
   G.iter_vertex (fun a -> GI.add_vertex imp a) pers;
   G.iter_edges_e (fun a -> GI.add_edge_e imp a) pers;
   (* GI.iter_vertex (fun a -> print_endline @@ id_of_node a) imp; *)
   imp

let dominaors (graph : G.t) 
              (start : node)
              ?(g_pred=GI.pred) 
              ?(g_succ=GI.succ)
              (vs: node list): (node * node) list = 
              

   let module Dom = Graph.Dominator.Make_graph (struct 
      include GI 
      let succ = g_succ
      let pred = g_pred
   end) in

   let domg = Dom.compute_all (to_impertive graph) start in  
 
   let check_dom acc curr = 
      domg.dominators curr 
         |> fun a -> List.map (fun v -> (v, curr)) a
         |> List.append acc 
   in
   
   List.fold_left check_dom [] vs

