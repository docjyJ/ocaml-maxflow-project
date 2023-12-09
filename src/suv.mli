open Graph

(* Type de flow qui enregistre la capacité et sont utilisation actuel *)
type flow

(* Convertie une chaine de caractère en flow *)
val flow_of_string: string -> flow

(* Convertie un flow en chaine de caractère *)
val string_of_flow: flow -> string

(* Trouve un chemin et le remplis *)
val step_flow: flow graph -> (id*id) -> flow graph

(* Remplis le graph jusqu'à ce que ce ne soit plus possible *)
val resolve_flow: flow graph -> (id*id) -> flow graph

(* permet de gardé un historique de toute les itération *)
val resolve_flow_with_step_list: flow graph -> (id*id) -> flow graph list

