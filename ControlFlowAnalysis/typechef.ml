open Base 
open Stdio
open VariabilityTypes
open Lexer

let qLog transformer target = 
    transformer target 
        |> Sexp.to_string 
        |> print_endline

type world = string * string [@@deriving sexp]

type nodeType = 
    | Declaration
    | Statement
    | Function
    | FunctionInline
    | Unparsed
 [@@deriving sexp]

            
type edge = Edge of {edge_id: string; a: node; b: node; varEdge: varE;}  [@@deriving sexp]
and node = Node of {nodeID: string; edges: edge list; varNode: varE; typeNode: nodeType; nodeValue: string}  [@@deriving sexp]

type cfg = (string, node, String.comparator_witness) Map.t *
           (string, edge list, String.comparator_witness) Map.t

let readFile = In_channel.read_lines  

let voidNode = Node {nodeID="0";  varNode= AtomV(""); typeNode= Statement; nodeValue= ""; edges= [] }


let parseNodeType = function
    | "statement" -> Statement
    | "function" -> Function
    | "function-inline" -> FunctionInline
    | "declaration" -> Declaration
    | (_ : string) -> Unparsed
    

let parseVariability (input : string) : varE = 
    qLog sexp_of_string input;
    if (String.equal input "1") then NoVar(()) else

    let parse_with_error lexbuf =
        try Parser.prog Lexer.read lexbuf with
        | SyntaxError msg ->
            eprintf  "syntax error: %s on string: %s\n" msg input;
            None
        | Parser.Error ->
            eprintf  "parser error\n";
            Caml.exit (-1)
    in

    Lexing.from_string input 
        |> parse_with_error
        |> function | Some (a) -> a
                    | _ -> AtomV("Unparsed")

let parseNode tokens : node = 
    Node {
            nodeID = List.nth_exn tokens 1; 
            edges = [];
            varNode =  List.nth_exn tokens 5 |> parseVariability; 
            typeNode = List.nth_exn tokens 2 |> parseNodeType ; 
            nodeValue = List.nth_exn tokens 4; 
    }

let parseEdge nodes (tokens : string sexp_list) : edge = 
    Edge {
        edge_id =  List.nth_exn tokens 1 ^ List.nth_exn tokens 2;
        a = List.nth_exn tokens 1 |> Map.find_exn nodes;
        b = List.nth_exn tokens 2 |> Map.find_exn nodes;
        varEdge =  List.nth_exn tokens 3 |> parseVariability
    }


let addNode (nodes, edges) (n: node) : cfg = 
    let id : string = match n with Node {nodeID=id; _} -> id in
    (Map.add_exn (nodes) ~key:id ~data:n , edges)


let addEdgeToNode (e : edge) = function
    | Node {nodeID=id; edges=es; varNode = vn ; typeNode = tn; nodeValue= nv} -> 
        Node {      
            nodeID=id;
            edges= e :: es;
            varNode = vn;
            typeNode = tn;
            nodeValue = nv;
        }

let addEdge ((nodes, edges) : cfg) e : cfg= 
    let id : string = match e with Edge {edge_id=id; _} -> id in
    let a : node = match e with Edge {a=a; _} -> a in
    let b : node = match e with Edge {b=b; _} -> b in

    let new_nodes = 
        nodes 
            |> fun m -> addEdgeToNode e a 
                |> fun d -> Map.set m ~key:(match a with Node {nodeID = id; _} -> id) ~data:d
            |> fun m -> addEdgeToNode e b 
                |> fun d -> Map.set m ~key:(match b with Node {nodeID = id; _} -> id) ~data:d
    in

    let updateEdge = function
        | Some(curr) -> e :: curr;
        | _ -> [e];
    in

    (new_nodes , Map.update (edges) id ~f:updateEdge)

    

let parseLine 
    ((nodes, edges) : cfg)
    (line: string) : cfg = 

    let tokens = String.split line ~on:';' in
    (* qLog (sexp_of_list sexp_of_string) tokens;
    
    tokens 
        |> List.length 
        |> (qLog sexp_of_int); *)

    match List.hd tokens with 
        | Some "N" -> tokens |> parseNode |> addNode (nodes, edges)
        | Some "E" -> tokens |> parseEdge nodes |> addEdge (nodes, edges) 
        | _ -> (nodes, edges) 

   

let buildCFG (input : string sexp_list)  = 
    let initial_nodes = Map.empty (module String) in
    let initial_edges = Map.empty (module String) in

    let n =  Node {nodeID="0"; edges= []; varNode= AtomV(""); typeNode= Statement; nodeValue= List.hd_exn input } in
    let updated = Map.add_exn initial_nodes ~key:"f" ~data:n in
    ignore updated;

    let parsed = List.fold input ~init:(initial_nodes, initial_edges) ~f:parseLine in 
    parsed


let () = 
    "./typechef_cfgs/i2c.cfg" 
        |> readFile 
        |> buildCFG 
        |> fun (a, _) -> Map.find_exn a "706322686"
        |> qLog sexp_of_node 
        |> ignore
    

