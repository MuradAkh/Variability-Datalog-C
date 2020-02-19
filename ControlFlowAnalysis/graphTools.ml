open Typechef


(* Thank you lambdapower @ Stackoverflow !! *)
(* https://stackoverflow.com/questions/8999557/how-to-visualize-draw-automata-in-ocaml/9011334#9011334 *)

(* representation of a node -- must be hashable *)
module GNode = struct
   type t = string
   let compare = Pervasives.compare
   let hash = Hashtbl.hash
   let equal = (=)
end

(* representation of an edge -- must be comparable *)
module GEdge = struct
   type t = string
   let compare = Pervasives.compare
   let equal = (=)
   let default = ""
end

(* a functional/persistent graph *)
module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled(GNode)(GEdge)

module Dom = Graph.Dominator.Make(G)


module Dot = Graph.Graphviz.Dot(struct
   include G (* use the graph module from above *)
   let edge_attributes (_, e, _) = [`Label e; `Color 4711]
   let default_edge_attributes _ = []
   let get_subgraph _ = None
   let vertex_attributes _ = [`Shape `Box]
   let vertex_name v = v
   let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let make_graph (nodes: node list) : G.t = 

   let strings_pair_nodes ((a, b) : node * node) : string * string = 
      let Node {nodeID = n1; _} = a in 
      let Node {nodeID = n2; _} = b in 
      (n1, n2)
   in

   let initial = G.empty in 
   let edges : (string * string) list =  
               List.map (function | Node {succs = e; _} -> e) nodes 
               |> List.concat 
               |> List.map (function | Edge {a = p; b = s;  _} -> (p, s))
               |> List.map strings_pair_nodes
   in

   List.map (function | Node {nodeID = id; _} -> id) nodes
      |> List.fold_left (fun acc curr -> G.add_vertex acc curr) initial
      |> fun g -> List.fold_left (fun acc (a, b) -> G.add_edge acc a b) g edges


let plot (graph : G.t) = 
   let file = open_out_bin "mygraph.dot" in
   Dot.output_graph file graph; 


   

