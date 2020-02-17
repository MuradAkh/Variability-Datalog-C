open Base
open TypechefTypes

type datalog_fact = DatalogFact of {variability : varE option; name: string; variables: string list} [@@deriving sexp]



let soufleString (fact : datalog_fact) : string = 
  fact |> ignore;
  ""