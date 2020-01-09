open Base 
open Stdio


let qLog transformer target = 
    transformer target 
        |> Sexp.to_string 
        |> print_endline

type world = string * string [@@deriving sexp]

type nodeType = 
    | Declaration
    | Statement
 [@@deriving sexp]

type  orV = varE * varE  [@@deriving sexp]
and andV = varE * varE  [@@deriving sexp]
and notV = varE  [@@deriving sexp]
and varE = CorV of string | AndV of andV | NotV of notV | OrV of orV  [@@deriving sexp]
            
type edge = Edge of {edge_id: string; a: node; b: node; varEdge: varE;}  [@@deriving sexp]
and node = Node of {node_id: string; edges: edge list; varNode: varE; typeNode: nodeType; nodeValue: string}  [@@deriving sexp]

type cfg = (string, node, String.comparator_witness) Map.t *
           (string, edge, String.comparator_witness) Map.t

let readFile = In_channel.read_lines  


let parseVariability (input : string) : varE = 
    (* TODO *)
    ignore input;
    CorV("")

let parseNode tokens : node = 
    Node {
            node_id = List.nth_exn tokens 1; 
            edges = [];
            varNode = "" |> parseVariability; 
            typeNode = Statement; 
            nodeValue = List.nth_exn tokens 1; 
    }

let parseEdge nodes (tokens : string sexp_list) : edge = 
    ignore tokens;
    Edge {
        edge_id =  List.nth_exn tokens 2 ^ List.nth_exn tokens 1;
        a = List.nth_exn tokens 2 |> Map.find_exn nodes;
        b = List.nth_exn tokens 1 |> Map.find_exn nodes;
        varEdge = "" |> parseVariability
    }


let addNode (nodes, edges) (n: node) : cfg = 
    let id : string = match n with Node {node_id=id; _} -> id in
    (Map.add_exn (nodes) ~key:id ~data:n , edges)


let addEdgeToNode (e : edge) (n: node) : node =
    ignore e;
    ignore n;
    parseNode []

let addEdge (nodes, edges) e = 
    let id : string = match e with Edge {edge_id=id; _} -> id in
    let a : node = match e with Edge {a=a; _} -> a in
    let b : node = match e with Edge {b=b; _} -> b in

    let new_nodes = 
        nodes 
            |> fun m -> addEdgeToNode e a 
                |> fun d -> Map.set m ~key:(match a with Node {node_id = id; _} -> id) ~data:d
            |> fun m -> addEdgeToNode e b 
                |> fun d -> Map.set m ~key:(match b with Node {node_id = id; _} -> id) ~data:d
    in

    (new_nodes , Map.add_exn (edges) ~key:id ~data:e)

    

let parseLine 
    ((nodes, edges) : cfg)
    (line: string) : cfg = 

    let tokens = String.split line ~on:';' in
    qLog (sexp_of_list sexp_of_string) tokens;
    
    tokens 
        |> List.length 
        |> (qLog sexp_of_int);

    match List.hd tokens with 
        | Some "N" -> tokens |> parseNode |> addNode (nodes, edges)
        | Some "E" -> tokens |> parseEdge nodes |> addEdge (nodes, edges) 
        | _ -> (nodes, edges) 

   

let buildCFG (input : string sexp_list)  = 
    let initial_nodes = Map.empty (module String) in
    let initial_edges = Map.empty (module String) in

    let n =  Node {node_id="0"; edges= []; varNode= CorV(""); typeNode= Statement; nodeValue= List.hd_exn input } in
    let updated = Map.add_exn initial_nodes ~key:"f" ~data:n in
    ignore updated;

    let parsed = List.fold input ~init:(initial_nodes, initial_edges) ~f:parseLine in 
    parsed


let () = 
    "/home/murad/typechefdump/d.cfg" 
        |> readFile 
        |> buildCFG 
        |> ignore
        (* |> fun (a, _) -> Map.find_exn a "786466608" *)
        (* |> qLog sexp_of_node  *)
    

