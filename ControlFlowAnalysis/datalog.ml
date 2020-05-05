open Base
open TypechefTypes

type datalog_fact = 
    | Dominator of {variability : varE option; doms: string; domed: string}
    | PostDominator of {variability : varE option; doms: string; domed: string}
    | Store of {variability : varE option; variable: string; node : string}
    | Load of {variability : varE option; variable: string; node : string}
    | PointsTo of {variability : varE option; variable: string; heap : string}
    | Assign of {variability : varE option; tovar: string; fromvar : string}
    [@@deriving sexp]



let soufleString (fact : datalog_fact) : string = 
  fact |> ignore;
  ""