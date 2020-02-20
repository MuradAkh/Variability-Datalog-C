open Base
open TypechefTypes

type datalog_fact = 
    | Dominator of {variability : varE option; doms: string; domed: string}
    | PostDominator of {variability : varE option; doms: string; domed: string}
    [@@deriving sexp]



let soufleString (fact : datalog_fact) : string = 
  fact |> ignore;
  ""