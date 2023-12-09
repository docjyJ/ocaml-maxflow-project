open Graph

exception No_Path of id list

let node_fold f acu g = n_fold g f acu
let node_iter f g = n_iter g f
let arc_fold f acu g = e_fold g f acu
let arc_iter f g = e_iter g f

let clone_nodes g = node_fold new_node empty_graph g

let label_map f g =
  let loop acu a = new_arc acu {a with lbl=f a.lbl}
  in arc_fold loop (clone_nodes g) g

let find_lbl_arc g id1 id2 = match find_arc g id1 id2 with
  | None -> raise(Graph_error("Arc from " ^ string_of_int id1 ^ " to "  ^ string_of_int id2 ^ " not found in the graph."))
  | Some x -> x.lbl

let add_arc g id1 id2 n = new_arc g {src=id1; tgt=id2; lbl=n+(try find_lbl_arc g id1 id2 with _ -> 0)}

let unify_arc g_old g_new =
  let loop g a = new_arc g {a with lbl=find_lbl_arc g_new a.src a.tgt}
  in arc_fold loop (clone_nodes g_old) g_old

(* g représente le graph, a le point qui est en train d'être évalué,
   b le point d'arriver, acu est une liste qui retient la série de point par laquel on est passé*)
let  find_path f g a b =
  let rec arc_loop acu = function
    (* Si il n'y a plus d'arc à exploré on lève une exception avec la liste des neuds explorés *)
    | [] -> raise (No_Path acu)
    (* Si l'arc n'est pas utilisable *)
    | h::q when f h -> arc_loop acu q
    (* Si le prochain neud et l'arrivé on termine l'algorithm *)
    | h::_ when h.tgt == b -> [h]
    (* Si on est déjà passé par l'arc on l'ignore *)
    | h::q when List.mem h.tgt acu -> arc_loop acu q
    (* On avance, et si on trouve pas on reviens*)
    | h::q -> try h::(arc_loop (h.tgt::acu) (out_arcs g h.tgt))
      (*On réutilise l'acu qui a fail, comme ça on connait plus vite les points qui vont pas à b*)
      with No_Path failed_acu -> arc_loop failed_acu q
  in arc_loop [a] (out_arcs g a)