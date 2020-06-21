
let do_log =
    match Caml.Sys.getenv_opt "DO_LOG" with 
      | Some("TRUE") -> true
      | _ -> false

let qLog transformer target = 
  if do_log then
  transformer target 
      |> Base.Sexp.to_string 
      |> print_endline

let sLog target = 
  if do_log then
  Stdio.print_endline target

let lqLog transformer target = 
  if do_log then
  Base.sexp_of_list target
    |> transformer 
    |> Base.Sexp.to_string 
    |> Stdio.print_endline

let exec maybe =
  if do_log then Lazy.force maybe