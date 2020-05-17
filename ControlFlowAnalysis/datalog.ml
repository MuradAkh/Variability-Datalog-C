open Base
open TypechefTypes

type generic_fact = GFact of {name: string; variability: varE option; parametrs: string list} 
[@@deriving sexp]

type datalog_fact = 
    | Dominator of {variability : varE option; doms: string; domed: string}
    | PostDominator of {variability : varE option; doms: string; domed: string}
    | Store of {variability : varE option; variable: string; node : string}
    | Load of {variability : varE option; variable: string; node : string}
    | PointsTo of {variability : varE option; variable: string; heap : string}
    | Assign of {variability : varE option; tovar: string; fromvar : string}
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
  | PointsTo {variability=var; variable=p1; heap=p2} -> 
      GFact {name= "pointsTo"; variability=var; parametrs=[p1;p2]}
  | Assign {variability=var; tovar=p1; fromvar=p2} -> 
      GFact {name= "assign"; variability=var; parametrs=[p1;p2]}

let brackets_surround target = "(" ^ target ^ ")"

let rec variabilityPrinter = function
  | AtomV(s) -> s
  | AndV(s) -> List.map ~f:variabilityPrinter s
    |> String.concat ~sep:"/\\" 
    |> brackets_surround
  | OrV(s) -> List.map ~f:variabilityPrinter s
    |> String.concat ~sep:"\\/"
    |> brackets_surround
  | NotV(s) -> "!" ^ variabilityPrinter s
  | NoVar()-> ""

let variabilityPrinterOption = function
  | Some(var: varE) -> variabilityPrinter var
  | None -> ""

let coreFactPrinter (name: string) (params: string list) : string = 
  List.map ~f:(fun a-> "\"" ^ a ^ "\"") params 
  |> fun a -> name ^  "(" ^ (String.concat ~sep:"," a) ^ ")"
  

let factPrinter = function 
  GFact {variability=var; parametrs=p; name=n} -> 
    coreFactPrinter n p ^ " @ " ^ variabilityPrinterOption var ^ "."
  