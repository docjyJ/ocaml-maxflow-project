(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

val clone_nodes: 'a graph -> 'b graph

val label_map: 'a graph -> ('a -> 'b) -> 'b graph

val add_arc: int graph -> id -> id -> int -> int graph

(* Récupère le babel associé à l'arc
 * @raise Graph_error si l'arc n'existe pas. *)
val find_lbl_arc: 'a graph -> id -> id -> 'a


val unify_arc: 'a graph -> 'b graph -> 'a graph