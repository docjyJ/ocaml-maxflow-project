open Graph
open Tools

let int_int_of_string i = (0, int_of_string i)

let init gr = gmap gr int_int_of_string

exception No_Pass of id list

let get_max =
  let rec loop acu = function
    | {lbl=(a,b);_}::q -> loop (min acu (b-a)) q
    | [] -> acu
  in loop max_int



let  find_path =
 (* g représente le graph, a le point qui est en train d'être évalué,
    b le poibt d'arriver, l est une liste qui retient la série de point par laquel on est passé*)
let rec loop acu g a b =
  let rec arc_loop = function
    | [] -> raise No_Pass acu
    | {lbl=(flow, cap);_}::q when (cap-flow) == 0 -> arc_loop q                 (*Si l'arc est rempli*)
    | arc::q -> try arc.src::(find_path_acu g arc.tgt b (arc.src::acu))         (*On avance, es si on trouve pas on reviens*)
                with No_Pass failed_acu -> loop failed_acu g a b                (*On réutilise l'acu qui a fail, comme ça on connait plus vite les points qui vont pas à b*)
  in
  if a = b then []                                                  (*Si on est arrivé on renvoie juste l*)
           else if mem a acu then raise No_Pass acu                 (*Si on est déja passé par ce point on remonte*)
                             else arc_loop (out_arcs g a)           (*On évalue la liste des arcs sortant de a*)
in
    loop []
