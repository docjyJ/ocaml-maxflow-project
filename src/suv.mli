open Graph

exception No_Pass of id list

type flow_graph = (int * int) graph

type flow_arc = (int * int) arc

val init: string graph -> flow_graph

val graph_flow_to_str: flow_graph -> string graph

val get_max: flow_arc list -> int

val find_path: flow_graph -> id -> id -> flow_arc list

val add_flow: flow_graph -> flow_arc -> int -> flow_graph

val apply_path: flow_graph -> flow_arc list -> flow_graph

val step_flow: flow_graph -> id -> id -> flow_graph

val resolve_flow: flow_graph -> id -> id -> flow_graph

