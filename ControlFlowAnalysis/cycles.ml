(**************************************************************************)
(*  Copyright (C) 2012 Pietro Abate <pietro.abate@pps.jussieu.fr>         *)
(*  Copyright (C) 2012 Johannes Schauer <j.schauer@email.de>              *)
(*                                                                        *)
(*  This library is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Lesser General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version.                       *)
(**************************************************************************)

(* Source: https://github.com/josch/cycles_johnson_abate/blob/master/cycles_functional.ml *)


module G = GraphTools.GI

module SV = Set.Make(Base.Int)

let to_set l = List.fold_right SV.add l SV.empty ;;

let partition s w = fst(SV.partition (fun e -> e >= w) s);;


let extract_subgraph g s =
  let sg = G.create () in
  G.iter_edges (fun v1 v2 ->
    if SV.mem (G.V.hash v1) s then G.add_vertex sg v1;
    if SV.mem (G.V.hash v2) s then G.add_vertex sg v2;
    if SV.mem (G.V.hash v1) s && SV.mem (G.V.hash v2) s then
      G.add_edge sg v1 v2
  ) g;
  sg
;;

let stack_to_list s = 
  let l = ref [] in
  Stack.iter (fun e -> l:= e::!l) s;
  !l
;;

type block = {
  blocked : (int,bool) Hashtbl.t;
  notelem : (int,G.V.t list) Hashtbl.t
}



let init_block g =
  let t = {
    blocked = Hashtbl.create 1023;
    notelem = Hashtbl.create 1023;
  } in
  G.iter_vertex (fun node ->
    Hashtbl.add t.blocked (G.V.hash node) false;
    Hashtbl.add t.notelem (G.V.hash node) [];
  ) g;
  t
;;

let rec unblock t n =
  if Hashtbl.find t.blocked (G.V.hash n) then begin
    Hashtbl.replace t.blocked (G.V.hash n) false;
    List.iter (unblock t) (Hashtbl.find t.notelem (G.V.hash n));
    Hashtbl.replace t.notelem (G.V.hash n) [];
  end
;;

let block t n =
  Hashtbl.replace t.blocked (G.V.hash n) true
;;

let find_all_cycles_johnson g =
  let nodes = Hashtbl.create 1023 in
  G.iter_vertex (fun v -> Hashtbl.add nodes (G.V.hash v) v) g;

  if not G.is_directed then
    assert false;

  (*  stack of nodes in current path *)
  let path = Stack.create () in

  let rec circuit t result thisnode startnode component = 

    Stack.push thisnode path;
    block t thisnode;

    let (closed,result) =
      G.fold_succ (fun nextnode (c,r) ->
        if G.V.equal nextnode startnode then begin
          (true, (stack_to_list path)::r)
        end else begin
          if not(Hashtbl.find t.blocked (G.V.hash nextnode)) then begin
            let c2, r2 = circuit t r nextnode startnode component in
            (c || c2, r2)
          end else
            (c,r)
        end
      ) component thisnode (false,result)
    in
    if closed then begin
      unblock t thisnode
    end else
      G.iter_succ (fun nextnode ->
        let l = Hashtbl.find t.notelem (G.V.hash nextnode) in
        if not(List.mem thisnode l) then
          Hashtbl.replace t.notelem (G.V.hash nextnode) (thisnode::l)
      ) component thisnode;
    ignore(Stack.pop path);
    (closed, result)
  in

  let to_ints =
    List.map G.V.hash
  in

 (* Johnson's algorithm requires some ordering of the nodes. *)
  let vertex_set = G.fold_vertex (fun n -> SV.add @@ G.V.hash n) g SV.empty in
  let result = SV.fold (fun s result ->
    (* Build the subgraph induced by s and following nodes in the ordering *)
    let subset = SV.add s (partition vertex_set s) in
    let subgraph = extract_subgraph g subset in

    (* Find the strongly connected component in the subgraph
     * that contains the least node according to the ordering *)
    let module Comp = Graph.Components.Make(G) in
    let scc = Comp.scc_list subgraph in

    if List.length scc = 0 then result else 
    
    let minnode = SV.min_elt subset in
    let mincomp = List.find (fun (l :G.V.t list) -> List.mem minnode (to_ints l)) scc in

    (* smallest node in the component according to the ordering *)
    let component = extract_subgraph subgraph (to_set @@ to_ints mincomp) in

    if G.nb_edges component > 0 then begin
      (* init the block table for this component *)
      let t = init_block component in

      snd(circuit t result (Hashtbl.find nodes minnode) (Hashtbl.find nodes minnode) component);
    end else
      result
  ) vertex_set []
  in
  List.rev result
;;

