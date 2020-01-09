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
            
type edge = Edge of {id: string; a: node; b: node; varEdge: varE;}  [@@deriving sexp]
and node = Node of {id: string; edges: edge list; varNode: varE; typeNode: nodeType; nodeValue: string}  [@@deriving sexp]



let readFile = In_channel.read_lines  

let parseNode tokens : node = 
    Node {id=List.hd_exn tokens; edges= []; varNode= CorV(""); typeNode= Statement; nodeValue= List.hd_exn tokens }

let parseEdge tokens : edge = 
    Edge {id= List.hd_exn tokens; a=parseNode []; b=parseNode []; varEdge= CorV("")}


let addNode nodes_edges n = 
    ignore n;
    nodes_edges

let addEdge nodes_edges e = 
    ignore e;
    nodes_edges
    

let parseLine 
    (nodes_edges : 
        (string, 'a, String.comparator_witness) Map.t *
        (string, 'b, String.comparator_witness) Map.t
    )
    (line: string) = 

    let tokens = String.split line ~on:';' in
    qLog (sexp_of_list sexp_of_string) tokens;
    
    tokens 
        |> List.length 
        |> (qLog sexp_of_int);

    match List.hd tokens with 
        | Some "N" -> addNode nodes_edges tokens
        | Some "E" -> addEdge nodes_edges tokens
        | _ -> nodes_edges




let buildCFG (input : string sexp_list)  = 
    let initial_nodes = Map.empty (module String) in
    let initial_edges = Map.empty (module String) in

    let n =  Node {id="0"; edges= []; varNode= CorV(""); typeNode= Statement; nodeValue= List.hd_exn input } in
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
    

