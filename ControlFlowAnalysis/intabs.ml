open Typechef
open Base

let () = 
    "./typechef_cfgs/i2c.cfg" 
        |> parseCfg 
        |> function ControlFlowGraph {nodes = n; edges= e; functions= f} -> (n, e, f)
        |> fun (a, _, _) -> Map.find_exn a "706322686"
        |> qLog sexp_of_node 
        |> ignore