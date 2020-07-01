open Typechef
open TypechefTypes
open Datalog
open CfgAnalysis
open Base

let get_analysis = match Sys.argv.(1) with
        | "POINTER" -> pointer_analysis_of_func
        | "STORE_LOAD" -> store_loads_of_func
        | "DOMINATOR" -> dominance
        | "CYCLES" -> cycles
        | _ -> pointer_analysis_of_func

let get_analysis_ast = match Sys.argv.(1) with
        | _ -> fun a -> Stdio.print_endline @@ Sexp.to_string @@ sexp_of_c_ast a

let plot n = Map.to_alist n      
        |> List.map ~f:(fun (_, b) -> b)
        |> GraphTools.make_graph
        |> GraphTools.plot

let do_ast _ = 
        "./_temp/output.cfg.ast" 
        |> Stdio.In_channel.read_all
        |> parseAst
        |> get_analysis_ast



let do_cfg _ = 
        "./_temp/output.cfg" 
        |> parseCfg 
        |> function ControlFlowGraph {nodes = n; edges= e; functions= f} -> Logger.sLog "parsed"; plot n; (n, e, f)
        |> fun (n, _, functions) -> functions
        |> Map.iter ~f:(
                fun func -> Logger.sLog @@ "analyzing: " ^ func ;Map.find_exn n func
                |> get_analysis
                |> List.map ~f:typed_to_generic
                |> List.map ~f:factPrinter
                |> List.iter ~f:Stdio.print_endline
        ) 

        (* |> List.iter ~f:(qLog sexp_of_datalog_fact) *)
        (* |> fun a -> Stdio.print_endline @@ Int.to_string @@ List.length a *)
        

let () = 
        do_cfg ();
        do_ast ();



        
        

    


