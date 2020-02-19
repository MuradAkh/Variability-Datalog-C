open Typechef
open Base
open Datalog
open GraphTools

let () = 
        let fact = DatalogFact {name=""; variables= [""];  variability= Some(AtomV(""))} in
        (soufleString fact) |> ignore;
        

        "./typechef_cfgs/blink1.cfg" 
        |> parseCfg 
        |> function ControlFlowGraph {nodes = n; edges= e; functions= f} -> (n, e, f)
        |> fun (n, _, _) -> Map.to_alist n |> List.map ~f:(fun (_, b) -> b)
        |> make_graph
        |> plot
        
        

    


