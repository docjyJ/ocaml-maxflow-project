open Graph

exception No_Pass of id list


val init: string graph -> (int * int) graph

val get_max: (int * int) arc list -> int

val find_path: (int*int) graph -> id -> id -> (int*int) arc list