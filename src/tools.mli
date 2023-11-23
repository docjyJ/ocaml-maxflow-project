(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

val clone_nodes: 'a graph -> 'b graph

val gmap: 'a graph -> ('a -> 'b) -> 'b graph
