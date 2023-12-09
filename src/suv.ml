open Graph
open Tools

exception No_Path of id list

type flow = {
  (* Capacité *)
  cap: int ;
  (* Utilisé *)
  use: int ;
  (* Espace restant *)
  spc: int }

let flow_of_string str = let i = int_of_string str in {cap=i; use=0; spc=i}

let string_of_flow fl= (string_of_int fl.use) ^ "/" ^ (string_of_int fl.cap)

let min_flow i ar = min i ar.lbl.spc



(* g représente le graph, a le point qui est en train d'être évalué,
   b le point d'arriver, acu est une liste qui retient la série de point par laquel on est passé*)
let  find_path g (a, b) =
  let rec arc_loop acu = function
    (* Si il n'y a plus d'arc à exploré on lève une exception avec la liste des neuds explorés *)
    | [] -> raise (No_Path acu)
    (* Si l'arc est rempli on l'ignore *)
    | h::q when h.lbl.spc == 0 -> arc_loop acu q
    (* Si le prochain neud et l'arrivé on termine l'algorithm *)
    | h::_ when h.tgt == b -> [h]
    (* Si on est déjà passé par l'arc on l'ignore *)
    | h::q when List.mem h.tgt acu -> arc_loop acu q
    (* On avance, et si on trouve pas on reviens*)
    | h::q -> try h::(arc_loop (h.tgt::acu) (out_arcs g h.tgt))
      (*On réutilise l'acu qui a fail, comme ça on connait plus vite les points qui vont pas à b*)
      with No_Path failed_acu -> arc_loop failed_acu q
  in arc_loop [a] (out_arcs g a)

let add_flow g {src=src;tgt=tgt;lbl={cap=cap;use=use;spc=spc}} i =new_arc (new_arc g {src=src;tgt=tgt;lbl={cap=cap;use=use+i;spc=spc-i}}) {src=tgt;tgt=src;lbl={cap=cap;use=spc-i;spc=use+i}}

let apply_path g path =
  let max_flow = List.fold_left min_flow max_int path
  in let rec loop acu = function
      | [] -> acu
      | h::q -> loop (add_flow acu h max_flow) q
  in loop g path



let resolve_flow input_graph point =
  let rec loop g = try
      loop (apply_path g (find_path g point))
    with No_Path _ -> g
  in unify_arc (loop input_graph) input_graph