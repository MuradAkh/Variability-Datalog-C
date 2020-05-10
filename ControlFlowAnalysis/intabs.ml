open Typechef
open Base
open Datalog
(* open GraphTools *)
(* open Stdio *)
open CfgAnalysis

let () = 
     

        "./typechef_cfgs/blink1.cfg" 
        |> parseCfg 
        |> function ControlFlowGraph {nodes = n; edges= e; functions= f} -> (n, e, f)
        
        |> fun (n, _, f) -> (Map.find_exn f "main", n)
        |> fun (f, n) -> Map.find_exn n f 
        |> fun a -> pointer_analysis_of_func a
        |> List.map ~f:typed_to_generic
        |> List.map ~f:factPrinter
        |> List.iter ~f:Stdio.print_endline
        (* |> List.iter ~f:(qLog sexp_of_datalog_fact) *)
        (* |> fun a -> Stdio.print_endline @@ Int.to_string @@ List.length a *)
        
        (* |> fun (n, _, _) -> Map.to_alist n |> List.map ~f:(fun (_, b) -> b)
        |> make_graph
        |> plot *)
        
        

    


