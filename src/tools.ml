open Graph
open Array

exception No_Path of id list

type  access = {
  (* Price *)
  mutable cost: int ;
  (*Access*)
  mutable acc: int ;
}

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


(*VA VOIR COMMENT FONCTION BELLMAN-FORD OU TU COMPRENDRA PAS*)
(*VA VOIR COMMENT FONCTION BELLMAN-FORD OU TU COMPRENDRA PAS*)
(*VA VOIR COMMENT FONCTION BELLMAN-FORD OU TU COMPRENDRA PAS*)
(*VA VOIR COMMENT FONCTION BELLMAN-FORD OU TU COMPRENDRA PAS*)
(*VA VOIR COMMENT FONCTION BELLMAN-FORD OU TU COMPRENDRA PAS*)
(*VA VOIR COMMENT FONCTION BELLMAN-FORD OU TU COMPRENDRA PAS*)

(*Si ca trouve jamais de chemin augmente la taille de la matrice mais normalement c'est ok*)

let find_path_Bell f1 f2 g a b =

    (* On crée et on initalise la matrice pour le point de départ
        de taille N * 15 (le 15 est choisie arbitrairement en esperant que ca suffise)*)
    let init_Bell g a =
        let m = make_matrix  15 (e_fold g (fun x _ -> x+1) 0) {cost=999;acc=(-1)} in
        ref m.(0).(a) := {cost=0;acc=(a)}; m
    in

    (* On met a jour toute une colone de la matrice pour ca au lieu d'iterer sur tous les point de la colone
        on itère sur tous les arcs (Ce qui est beaucoup plus rapide)
        On regarde si l'arc est valide par F1 et si son cout et avantageux par F2*)
    let rec maj f1 f2 m n l=
    match l with
    | [] -> ()
    | x::rest ->
        if (( not (f1 x)) && (m.(n).(x.tgt).cost > (m.(n-1).(x.src).cost + (f2 x))))
            then ref (m.(n).(x.tgt)) := {cost=(m.(n-1).(x.src).cost + (f2 x)); acc = x.src};
        maj f1 f2 m n rest
    in

    (*Sert a retrouver le chemin final à partir de la matrice
        renvoi une liste d'arc*)
    let rec path m g n a acu =
        if n<0
            then acu
            else match find_arc g a m.(n).(a).acc with
            | None -> assert false
            | Some x -> path m g (n-1) (m.(n).(a).acc) (x::acu)
    in

    (*On regarde si la matrice est immobile, si oui on a fini,
        sinon on met à jour la prochaine colone et on recommence
        si on est a la fin du tableau et qu'on est pas stable alors on a pas trouver*)
    let rec grosse_maj f1 f2 g b m n =

        let liste_arc g = e_fold g (fun l x -> x::l) [] in
        let step = maj f1 f2 m n (liste_arc g); grosse_maj f1 f2 g b m (n+1) in

        if m.(n) = m.(n-1)
            then path m g n b []
            else if n = 15
                then (No_Path [])
                else step
    in

    grosse_maj f1 f2 g b (init_Bell g a) 1