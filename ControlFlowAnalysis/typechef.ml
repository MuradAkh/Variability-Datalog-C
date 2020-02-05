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

            
type expression = Expression of {value: string; container: string option} [@@deriving sexp]

type edge = Edge of {edge_id: string; a: node; b: node; varEdge: varE;}  [@@deriving sexp]
and node = Node of {nodeID: string; edges: edge list; varNode: varE; typeNode: nodeType; nodeValue: expression}  [@@deriving sexp]

type cfg = (string, node, String.comparator_witness) Map.t *
           (string, edge list, String.comparator_witness) Map.t

let readFile = In_channel.read_lines  

let voidNode = Node {nodeID="0";  varNode= AtomV(""); typeNode= Statement; nodeValue= Expression {value=""; container=Some("")}; edges= [] }

type flowGraph = ControlFlowGraph of {
                                        nodes: (string, node, String.comparator_witness) Map.t;
                                        edges: (string, edge list, String.comparator_witness) Map.t;
                                        functions: (string, node, String.comparator_witness) Map.t;
                                     }

let parseCfg (filepath : string) = 

    let parseNodeType = function
        | "statement" -> Statement
        | "function" -> Function
        | "function-inline" -> FunctionInline
        | "declaration" -> Declaration
        | (_ : string) -> Unparsed
    in
        

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
    in

    let parseExpression (exp: string) : expression = exp
        |> String.substr_replace_all ~pattern:"::" ~with_:"%" 
        |> String.split ~on:'%'
        |> fun a -> print_endline exp; Expression {value=List.hd_exn a; container=List.nth a 1}
    in

    let parseNode tokens : node = 
        Node {
                nodeID = List.nth_exn tokens 1; 
                edges = [];
                varNode =  List.nth_exn tokens 5 |> parseVariability; 
                typeNode = List.nth_exn tokens 2 |> parseNodeType ; 
                nodeValue = List.nth_exn tokens 4 |> parseExpression; 
        }
    in

    let parseEdge nodes (tokens : string sexp_list) : edge = 
        Edge {
            edge_id =  List.nth_exn tokens 1 ^ List.nth_exn tokens 2;
            a = List.nth_exn tokens 1 |> Map.find_exn nodes;
            b = List.nth_exn tokens 2 |> Map.find_exn nodes;
            varEdge =  List.nth_exn tokens 3 |> parseVariability
        }
    in

    let addNode (nodes, edges) (n: node) : cfg = 
        let id : string = match n with Node {nodeID=id; _} -> id in
        (Map.add_exn (nodes) ~key:id ~data:n , edges)
    in

    let addEdgeToNode (e : edge) = function
        | Node {nodeID=id; edges=es; varNode = vn ; typeNode = tn; nodeValue= nv} -> 
            Node {      
                nodeID=id;
                edges= e :: es;
                varNode = vn;
                typeNode = tn;
                nodeValue = nv;
            }
    in

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
    in
        

    let parseLine (nodes, edges, functions) (line: string) = 

        let tokens = String.split line ~on:';' in
        (* qLog (sexp_of_list sexp_of_string) tokens;
        tokens 
            |> List.length 
            |> (qLog sexp_of_int); *)

        let addFunction (n : node)  = 
            let Node {typeNode = nodeType; nodeValue = nodeVal; _} = n in 
            let Expression {value = expVal; _} = nodeVal in
            match nodeType with 
                | Function -> Map.set functions ~key:expVal ~data:n 
                | _ -> functions;
        in

        
        match List.hd tokens with 
            | Some "E" -> tokens |> parseEdge nodes |> addEdge (nodes, edges) |> fun (n, e) -> (n, e, functions)
            | Some "N" -> tokens |> parseNode 
                                |> fun n -> (addNode (nodes, edges) n, addFunction n)
                                |> fun ((n, e), f) -> (n, e, f)
            | _ -> (nodes, edges, functions) 
    in
    

    let buildCFG (input : string sexp_list)  = 
        let initial_nodes = Map.empty (module String) in
        let initial_edges = Map.empty (module String) in
        let initial_functions = Map.empty (module String) in
    (* 
        let n =  Node {nodeID="0"; edges= []; varNode= AtomV(""); typeNode= Statement; nodeValue= List.hd_exn input } in
        let updated = Map.add_exn initial_nodes ~key:"f" ~data:n in
        ignore updated; *)

        let parsed = List.fold input ~init:(initial_nodes, initial_edges, initial_functions) ~f:parseLine in 
        parsed
    in

    filepath |> readFile |> buildCFG |> fun (n, e, f) -> ControlFlowGraph {nodes = n; edges= e; functions= f}
 
let () = 
    "./typechef_cfgs/i2c.cfg" 
        |> parseCfg 
        |> function ControlFlowGraph {nodes = n; edges= e; functions= f} -> (n, e, f)
        |> fun (a, _, _) -> Map.find_exn a "706322686"
        |> qLog sexp_of_node 
        |> ignore


