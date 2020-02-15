open Base
open TypechefTypes

type datalog_fact = DatalogFact of {clause: string; variability : varE} [@@deriving sexp]



let soufleString (fact : datalog_fact) : string = 
  fact |> ignore;
  ""