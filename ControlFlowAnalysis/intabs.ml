open Typechef
open Base
open Datalog

let () = 
        let fact = DatalogFact {clause=""; variability= AtomV("")} in
        (soufleString fact) |> ignore;

        "./typechef_cfgs/blink1.cfg" 
        |> parseCfg 
        |> function ControlFlowGraph {nodes = n; edges= e; functions= f} -> (n, e, f)
        |> fun (_, _, f) -> Map.find_exn f "main"
        |> qLog sexp_of_node 
        |> ignore
        
        

    


