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
 b le poibt d'arriver, acu est une liste qui retient la série de point par laquel on est passé*)
let  find_path g (first, b)=
  let rec next_node acu a =
    let rec arc_loop acu2 = function
      (*Si il n'y a plus d'arc à exploré on lève une exeption avec la liste des neuds explorés*)
      | [] -> raise (No_Path acu2)
      (*Si l'arc est rempli on l'ignore ou
       *Si on est déjà passé par l'arc on l'ignore*)
      | h::q when (h.lbl.spc == 0) || (List.mem h.tgt acu2) -> arc_loop acu2 q
      (*On avance, es si on trouve pas on reviens*)
      | h::q -> try h::(next_node (h.src::acu2) h.tgt)
        (*On réutilise l'acu qui a fail, comme ça on connait plus vite les points qui vont pas à b*)
        with No_Path failed_acu -> arc_loop failed_acu q

    (*Si on est arrivé on renvoie juste l sinon on évalue la liste des arcs sortant de a*)
    in if a = b then [] else arc_loop acu (out_arcs g a)
  in next_node [] first

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