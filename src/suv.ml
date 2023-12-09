open Graph

exception No_Path of id list

type flow ={
(* Capacité *)
    cap: int ;
    (* Utilisé *)
    use: int ;
    (* Espace restant *)
    spc: int }

let flow_of_string str = let i = int_of_string str in {cap=i; use=0; spc=i}

let string_of_flow fl= (string_of_int fl.use) ^ "/" ^ (string_of_int fl.cap)



let min_flow i ar = min i ar.lbl.spc



let  find_path =
  (* g représente le graph, a le point qui est en train d'être évalué,
     b le poibt d'arriver, l est une liste qui retient la série de point par laquel on est passé*)
  let rec loop acu g a b =
    let rec arc_loop acu2 = function
      (*Si il n'y a plus d'arc à exploré on lève une exeption avec la liste des neuds explorés*)
      | [] -> raise (No_Path acu2)
      (*Si l'arc est rempli on l'ignore ou
       *Si on est déjà passé par l'arc on l'ignore*)
      | h::q when (h.lbl.spc == 0) || (List.mem h.tgt acu)-> arc_loop acu2 q
      (*On avance, es si on trouve pas on reviens*)
      | h::q -> try h::(loop (h.src::acu2) g h.tgt b)
        (*On réutilise l'acu qui a fail, comme ça on connait plus vite les points qui vont pas à b*)
        with No_Path failed_acu -> arc_loop failed_acu q
    in
    (*Si on est arrivé on renvoie juste l sinon on évalue la liste des arcs sortant de a*)
    if a = b then [] else arc_loop acu (out_arcs g a)
  in
  loop []

let add_flow g {src=src;tgt=tgt;lbl={cap=cap;use=use;spc=spc}} i =new_arc (new_arc g {src=src;tgt=tgt;lbl={cap=cap;use=use+i;spc=spc-i}}) {src=tgt;tgt=src;lbl={cap=cap;use=spc-i;spc=use+i}}

let apply_path g path =
  let max_flow = List.fold_left min_flow max_int path
  in let rec loop acu = function
      | [] -> acu
      | h::q -> loop (add_flow acu h max_flow) q
  in loop g path
  (*
let rec find_path_old g a b =
  let rec arc_loop = function
    | [] -> raise (No_Path [])
    | {lbl=(ia, ib);_}::q when ia == ib -> arc_loop q
    | arc::q -> try arc::(find_path_old g arc.tgt b)
      with No_Path _ -> arc_loop q
  in
  if a = b then [] else arc_loop (out_arcs g a)
*)
 let step_flow g a b = apply_path g (find_path g a b)

let rec resolve_flow g a b = try
    resolve_flow (step_flow g a b) a b
  with No_Path _ -> g


 let unify_arc g1 g2 =
 let find_lbl_arc arc graph =
     match find_arc graph arc.src arc.tgt with
     | None -> assert false
     | Some x -> x.lbl
 in
 let loop (graph1, graph2) arc = (Graph.new_arc graph1 {src=arc.src; tgt=arc.tgt; lbl= (find_lbl_arc arc graph2)}, graph2)
 in
    let (fin, _) = e_fold g1 loop ((clone_nodes g1), g2)
 in
    fin