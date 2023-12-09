(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

(* Exeption lorsqu'un chemin n'est pas trouvé dans un graph
 * Retourne la liste des neud exploré *)
exception No_Path of id list

(* Réécriture pour avoir la même définition que List.fold_left*)
val node_fold: ('acc -> id -> 'acc) -> 'acc -> 'a graph -> 'acc

(* Réécriture pour avoir la même définition que List.iter*)
val node_iter: (id -> unit) -> 'a graph -> unit

(* Réécriture pour avoir la même définition que List.fold_left*)
val arc_fold: ('acc -> 'a arc -> 'acc) -> 'acc -> 'a graph -> 'acc

(* Réécriture pour avoir la même définition que List.iter*)
val arc_iter: ('a arc -> unit) -> 'a graph -> unit

(* Crée un nouveaux graphe à partir des neud d'un autre graph *)
val clone_nodes: 'a graph -> 'b graph

(* Permet de map sur tout les label *)
val label_map: ('a -> 'b) -> 'a graph -> 'b graph

(* Récupère le babel associé à l'arc
 * @raise Graph_error si l'arc n'existe pas. *)
val find_lbl_arc: 'a graph -> id -> id -> 'a

(* Ajout n à l'arc a vers b, crée cette arc si il n'existe pas *)
val add_arc: int graph -> id -> id -> int -> int graph

(* Permet de copier les label d'un graph sur les arc d'un autre.
 * @raise Graph_error si il manque un arc. *)
val unify_arc: 'a graph -> 'b graph -> 'b graph

(* Permet de trouver un chemin sur un Graph
 * Il est possible d'ajouté une fonction qui permet de refusé le passage par un arc si elle est vrai.
 * @raise No_Path si il n'existe pas de chemin. *)
val find_path: ('a arc -> bool) -> 'a graph -> id -> id -> 'a arc list