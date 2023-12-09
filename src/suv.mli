open Graph

(* Type de flow qui enregistre la capacité et sont utilisation actuel *)
type flow

(* Exeption lorsqu'un chemin n'est pas trouvé dans un graph
 * Retourne la liste des neud exploré *)
exception No_Path of id list


(* Convertie une chaine de caractère en flow *)
val flow_of_string: string -> flow

(* Convertie un flow en chaine de caractère *)
val string_of_flow: flow -> string


val find_path: flow graph -> (id*id) -> flow arc list

val add_flow: flow graph -> flow arc -> int -> flow graph

val apply_path:  flow graph -> flow arc list -> flow graph

val resolve_flow: flow graph -> (id*id) -> flow graph


