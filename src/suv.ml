open Graph
open Tools

exception No_Pass of id list

type flow_graph = (int * int) graph

type flow_arc = (int * int) arc

let init g = gmap g (fun i -> (0, int_of_string i))

let graph_flow_to_str g = gmap g (fun (flow, cap) -> Printf.sprintf "%d/%d" flow cap)

let get_max = List.fold_left (fun i {lbl=(flow, cap);_} -> min i (cap-flow)) max_int



let  find_path =
  (* g représente le graph, a le point qui est en train d'être évalué,
     b le poibt d'arriver, l est une liste qui retient la série de point par laquel on est passé*)
  let rec loop acu g a b =
    let rec arc_loop acu2 = function
      | [] -> raise (No_Pass acu)
      | {lbl=(flow, cap);_}::q when (cap-flow) == 0 -> arc_loop acu2 q    (*Si l'arc est rempli*)
      | arc::q -> try arc::(loop (arc.src::acu2) g arc.tgt b)         (*On avance, es si on trouve pas on reviens*)
        with No_Pass failed_acu -> arc_loop failed_acu q             (*On réutilise l'acu qui a fail, comme ça on connait plus vite les points qui vont pas à b*)
    in
    if a = b then []                                                (*Si on est arrivé on renvoie juste l*)
    else if List.mem a acu then raise (No_Pass acu)                 (*Si on est déja passé par ce point on remonte*)
    else arc_loop acu (out_arcs g a)                                    (*On évalue la liste des arcs sortant de a*)
  in
  loop []

let add_flow g {src=src; tgt=tgt; lbl=(flow, cap)} i =
let new_flow = flow+i
in new_arc (new_arc g {src=src; tgt=tgt; lbl=(new_flow, cap)}) {src=tgt; tgt=src; lbl=(cap-new_flow, cap)}

let apply_path g path =
  let max_flow = get_max path
  in let rec loop acu = function
    | [] -> acu
    | h::q -> loop (add_flow acu h max_flow) q
  in loop g path
(*
let rec find_path_old g a b =
  let rec arc_loop = function
    | [] -> raise (No_Pass [])
    | {lbl=(ia, ib);_}::q when ia == ib -> arc_loop q
    | arc::q -> try arc::(find_path_old g arc.tgt b)
      with No_Pass _ -> arc_loop q
  in
  if a = b then [] else arc_loop (out_arcs g a)
*)
 let step_flow g a b = apply_path g (find_path g a b)

 let rec resolve_flow g a b = try
 resolve_flow (step_flow g a b) a b
 with No_Pass _ -> g