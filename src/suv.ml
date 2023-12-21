open Graph
open Tools

type flow = {
  (* Capacité *)
  cap: int ;
  (* Utilisé *)
  use: int ;
  (* Espace restant *)
  spc: int ;
  (* Cost *)
  cost: int
}

let flow_of_string str = let f use cap cost = {cap=cap; use=use; spc=cap-use; cost=(-cost)} in
  try Scanf.sscanf str "%d/%d.%d" f
  with _ -> try Scanf.sscanf str "%d/%d" (fun a b -> f a b 0)
    with _ -> try Scanf.sscanf str "%d.%d" (f 0)
      with _ -> Scanf.sscanf str "%d" (fun a -> f 0 a 0)
let string_of_flow fl= (string_of_int fl.use) ^ "/" ^ (string_of_int fl.cap) ^ "." ^ (string_of_int (-fl.cost))

let min_flow i ar = min i ar.lbl.spc
let is_full a = a.lbl.spc = 0
let is_empty a = a.lbl.use = 0
let get_cost a = a.lbl.cost
let add_flow i g arc =
  new_arc (new_arc g {arc with lbl={arc.lbl with use=arc.lbl.use+i;spc=arc.lbl.spc-i}})
    {src=arc.tgt;tgt=arc.src;lbl={arc.lbl with use=arc.lbl.spc-i;spc=arc.lbl.use+i;cost=(-arc.lbl.cost)}}
let create_revers gin =  let loop g a = add_flow 0 g a in arc_fold loop (clone_nodes gin) gin


let step_flow g (a,b) =
  (* On cherche le chemin le plus cours *)
  (*  let path = find_path is_full g a b *)
  let path = find_path_bell is_full get_cost g a b
  (* On regarde le flot maximal possible *)
  in let max_flow = List.fold_left min_flow max_int path
  (* On applique ce ce chemin *)
  in List.fold_left (add_flow max_flow) g path


let resolve_flow input_graph point =
  let rec loop g = try
      loop (step_flow g point)
    with No_Path _ -> unify_arc input_graph g
  in loop (create_revers input_graph)

let resolve_flow_with_step_list input_graph point =
  let rec loop g acu = try
      loop (step_flow g point) (g::acu)
    with No_Path _ -> List.map (unify_arc input_graph) (g::acu)
  in loop (create_revers input_graph) []
