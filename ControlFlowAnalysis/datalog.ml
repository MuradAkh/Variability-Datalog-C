open Base
open TypechefTypes

type generic_fact = GFact of {name: string; variability: varE option; parametrs: string list} 
[@@deriving sexp]

type datalog_fact = 
    | Dominator of {variability : varE option; doms: string; domed: string}
    | PostDominator of {variability : varE option; doms: string; domed: string}
    | Store of {variability : varE option; variable: string; node : string}
    | Load of {variability : varE option; variable: string; node : string}
    | VariableDeclaration of {variability : varE option; variable: string; node : string}
    | PointsTo of {variability : varE option; variable: string; heap : string}
    | Assign of {variability : varE option; tovar: string; fromvar : string}
    | Cycle of {variability : varE option; id : string}
    | NodeCycle of {variability : varE option; cycleId: string; nodeAId: string; nodeBId: string}
    | Edge of {variability : varE option; nodeAId: string; nodeBId: string}
    | AssignedReturn of {variability : varE option; variable: string; heap : string}
    | ReturnsPointer of {variability : varE option; variable: string;}
    [@@deriving sexp]

let typed_to_generic = function
  | Dominator {variability=var; doms=p1; domed=p2} -> 
      GFact {name= "dominator"; variability=var; parametrs=[p1;p2]}
  | PostDominator {variability=var; doms=p1; domed=p2} -> 
      GFact {name= "postDominator"; variability=var; parametrs=[p1;p2]}
  | Store {variability=var; variable=p1; node=p2} -> 
      GFact {name= "store"; variability=var; parametrs=[p1;p2]}
  | Load {variability=var; variable=p1; node=p2} -> 
      GFact {name= "load"; variability=var; parametrs=[p1;p2]}
  | VariableDeclaration {variability=var; variable=p1; node=p2} -> 
      GFact {name= "declare"; variability=var; parametrs=[p1;p2]}
  | PointsTo {variability=var; variable=p1; heap=p2} -> 
      GFact {name= "pointsTo"; variability=var; parametrs=[p1;p2]}
  | AssignedReturn {variability=var; variable=p1; heap=p2} -> 
      GFact {name= "assigned_func"; variability=var; parametrs=[p1;p2]}
  | Assign {variability=var; tovar=p1; fromvar=p2} -> 
      GFact {name= "assign"; variability=var; parametrs=[p1;p2]}
  | Cycle {variability=var; id=p1} -> 
      GFact {name= "cycle"; variability=var; parametrs=[p1]}
  | NodeCycle {variability=var; cycleId=p1; nodeAId=p2; nodeBId=p3} -> 
      GFact {name= "cycleNode"; variability=var; parametrs=[p1;p2;p3]}
  | Edge {variability=var; nodeAId=p2; nodeBId=p3} -> 
      GFact {name= "edge"; variability=var; parametrs=[p2;p3]}
  | ReturnsPointer {variability=var; variable=p} -> 
      GFact {name= "returns_pointer"; variability=var; parametrs=[p]}
  

let brackets_surround target = "(" ^ target ^ ")"

let rec variabilityPrinter = function
  | AtomV(s) -> s
  | AndV(s) -> List.map ~f:variabilityPrinter s
    |> String.concat ~sep:" /\\ " 
    |> brackets_surround
  | OrV(s) -> List.map ~f:variabilityPrinter s
    |> String.concat ~sep:" \\/ "
    |> brackets_surround
  | NotV(s) -> "!" ^ variabilityPrinter s
  | NoVar()-> ""

let variabilityPrinterOption = function
  | Some(var: varE) -> begin
    match var with 
    | NoVar() -> ""
    | _ -> " @ " ^ variabilityPrinter var end
  | None -> ""

let coreFactPrinter (name: string) (params: string list) : string = 
  List.map ~f:(fun a-> "\"" ^ a ^ "\"") params 
  |> fun a -> name ^  "(" ^ (String.concat ~sep:"," a) ^ ")"
  

let factPrinter = function 
  GFact {variability=var; parametrs=p; name=n} -> 
    coreFactPrinter n p ^ variabilityPrinterOption var ^ "."
  