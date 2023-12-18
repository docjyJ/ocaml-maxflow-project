open Graph
open Suv

type path = string
type node_dictionary

val read_file: path -> (node_dictionary * flow graph)

val export: path -> node_dictionary -> flow graph -> unit

val export_all: path -> node_dictionary -> flow graph list -> unit
