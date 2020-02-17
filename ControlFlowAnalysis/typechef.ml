open Base 
open Stdio
open TypechefTypes

let qLog transformer target = 
    transformer target 
        |> Sexp.to_string 
        |> print_endline

type world = string * string [@@deriving sexp]

(* type nodeType = 
    | Declaration
    | Statement
    | Function
    | FunctionInline
    | Unparsed
 [@@deriving sexp] *)

            
type expression = 
                | Declaration of {value: c_ast; container: string option} 
                | Statement of {value: c_ast; container: string option} 
                | Function of {value: string; container: string option} 
                | FunctionInline of {value: string; container: string option} 
                | Unparsed of {value: string; container: string option} 

                 [@@deriving sexp]

type edge = Edge of {edge_id: string; a: node; b: node; varEdge: varE;}  [@@deriving sexp]
and node = Node of {nodeID: string; succs: edge list; preds: edge list; varNode: varE;  nodeValue: expression}  [@@deriving sexp]

type cfg = (string, node, String.comparator_witness) Map.t *
           (string, edge list, String.comparator_witness) Map.t

let readFile = In_channel.read_lines  

let voidNode = Node {nodeID="0";  varNode= AtomV(""); nodeValue= Statement {value=AtomicAst(""); container=Some("")}; succs= []; preds =[] }

type flowGraph = ControlFlowGraph of {
                                        nodes: (string, node, String.comparator_witness) Map.t;
                                        edges: (string, edge list, String.comparator_witness) Map.t;
                                        functions: (string, node, String.comparator_witness) Map.t;
                                     }

let parseCfg (filepath : string) = 

    (* let parseNodeType = function
        | "statement" -> Statement
        | "function" -> Function
        | "function-inline" -> FunctionInline
        | "declaration" -> Declaration
        | (_ : string) -> Unparsed
    in *)
        

    let parseVariability (input : string) : varE = 
        qLog sexp_of_string input;
        if (String.equal input "1") then NoVar(()) else

        let parse_with_error lexbuf =
            try VarParser.prog VarLexer.read lexbuf with
            | VarLexer.SyntaxError msg ->
                eprintf  "syntax error: %s on string: %s\n" msg input;
                None
            | VarParser.Error ->
                eprintf  "parser error on\n";
                Caml.exit (-1)
        in

        Lexing.from_string input 
            |> parse_with_error
            |> function | Some (a) -> a
                        | _ -> AtomV("Unparsed")
    in

    let parseAst (exp: string) : c_ast =

        let parse_with_error lexbuf =
            try AstParser.prog AstLexer.read lexbuf with
            | AstLexer.SyntaxError msg ->
                eprintf  "syntax error: %s on string: %s\n" msg exp;
                None
            | AstParser.Error ->
                eprintf  "parser error on %s \n" exp;
                Caml.exit (-1)
        in

        Lexing.from_string exp 
            |> parse_with_error
            |> function | Some (a) -> a
                        | _ -> AtomicAst("Unparsed")

    
    in

    let parseExpression (expr_type: string) (exp: string) : expression = exp
        |> String.substr_replace_all ~pattern:"::" ~with_:"%" 
        |> String.split ~on:'%'
        |> fun (a : string list) -> 
            match expr_type with 
            | "declaration" -> Declaration {value=List.hd_exn a |> parseAst; container=List.nth a 1}
            | "statement" -> Statement {value=List.hd_exn a |> parseAst; container=List.nth a 1}
            | "function" -> Function {value=List.hd_exn a; container=List.nth a 1}
            | "function-inline" -> FunctionInline {value=List.hd_exn a; container=List.nth a 1}
            | _ -> Unparsed {value=List.hd_exn a; container=List.nth a 1}
    in

    let parseNode tokens : node = 
        Node {
                nodeID = List.nth_exn tokens 1; 
                succs = [];
                preds = [];
                varNode =  List.nth_exn tokens 5 |> parseVariability; 
                nodeValue = List.nth_exn tokens 4 |> parseExpression (List.nth_exn tokens 2); 
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

    let addPredToNode (e : edge) = function
        | Node {nodeID=id; succs=ss; preds=ps; varNode = vn ; nodeValue= nv} -> 
            Node {      
                nodeID=id;
                succs= ss;
                preds= e :: ps;
                varNode = vn;
                nodeValue = nv;
            }
    in

    let addSuccToNode (e : edge) = function
        | Node {nodeID=id; succs=ss; preds=ps; varNode = vn ; nodeValue= nv} -> 
            Node {      
                nodeID=id;
                succs= e :: ss;
                preds= ps;
                varNode = vn;
                nodeValue = nv;
            }
    in

    let addEdge ((nodes, edges) : cfg) e : cfg= 
        let id : string = match e with Edge {edge_id=id; _} -> id in
        let a : node = match e with Edge {a=a; _} -> a in
        let b : node = match e with Edge {b=b; _} -> b in

        let new_nodes = 
            nodes 
                |> fun m -> addPredToNode e b
                    |> fun d -> Map.set m ~key:(match a with Node {nodeID = id; _} -> id) ~data:d
                |> fun m -> addSuccToNode e a
                    |> fun d -> Map.set m ~key:(match b with Node {nodeID = id; _} -> id) ~data:d
        in

        let updateEdge = function
            | Some(curr) -> e :: curr;
            | _ -> [e];
        in

        let Node {nodeValue = nodeVal; _} = a in 
        match nodeVal with
            | Function(_) -> (nodes, edges) 
            | _ ->  (new_nodes , Map.update (edges) id ~f:updateEdge)


    in
        

    let parseLine (nodes, edges, functions) (line: string) = 

        let tokens = String.split line ~on:';' in
        (* qLog (sexp_of_list sexp_of_string) tokens;
        tokens 
            |> List.length 
            |> (qLog sexp_of_int); *)

        let addFunction (n : node)  = 
            let Node {nodeValue = nodeVal; _} = n in 
            match nodeVal with 
                | Function {value = expVal; _} -> Map.set functions ~key:expVal ~data:n 
                | _ -> functions;
        in

        
        match List.hd tokens with 
            | Some "E" -> tokens |> parseEdge nodes 
                                 |> addEdge (nodes, edges) 
                                 |> fun (n, e) -> (n, e, functions)

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

    filepath 
        |> readFile 
        |> buildCFG 
        |> fun (n, e, f) -> ControlFlowGraph {nodes = n; edges= e; functions= f}
 
(* let () = 
    "./typechef_cfgs/i2c.cfg" 
        |> parseCfg 
        |> function ControlFlowGraph {nodes = n; edges= e; functions= f} -> (n, e, f)
        |> fun (a, _, _) -> Map.find_exn a "706322686"
        |> qLog sexp_of_node 
        |> ignore *)


