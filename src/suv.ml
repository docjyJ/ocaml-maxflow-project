open Graph
open Tools

type flow = {
  (* Capacité *)
  cap: int ;
  (* Utilisé *)
  use: int ;
  (* Espace restant *)
  spc: int
}

let flow_of_string str = let f use cap = {cap=cap; use=use; spc=cap-use}
  in try Scanf.sscanf str "%d/%d" f
  with _ -> Scanf.sscanf str "%d" (f 0)
let string_of_flow fl= (string_of_int fl.use) ^ "/" ^ (string_of_int fl.cap)

let min_flow i ar = min i ar.lbl.spc
let is_full a = a.lbl.spc = 0
let add_flow i g {src=src;tgt=tgt;lbl={cap=cap;use=use;spc=spc}} =
  new_arc (new_arc g
             {src=src;tgt=tgt;lbl={cap=cap;use=use+i;spc=spc-i}})
    {src=tgt;tgt=src;lbl={cap=cap;use=spc-i;spc=use+i}}


let step_flow g (a,b) =
  (* On cherche le chemin le plus cours *)
  let path = find_path is_full g a b
  (* On regarde le flot maximal possible *)
  in let max_flow = List.fold_left min_flow max_int path
  (* On applique ce ce chemin *)
  in List.fold_left (add_flow max_flow) g path


let resolve_flow input_graph point =
  let rec loop g = try
      loop (step_flow g point)
    with No_Path _ -> unify_arc input_graph g
  in loop input_graph

let resolve_flow_with_step_list input_graph point =
  let rec loop g acu = try
      loop (step_flow g point) (g::acu)
    with No_Path _ -> List.map (unify_arc input_graph) (g::acu)
  in loop input_graph []