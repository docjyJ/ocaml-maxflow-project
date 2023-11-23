

let clone_nodes gr = Graph.n_fold gr Graph.new_node Graph.empty_graph

(*let gmap gr f = Graph.e_fold gr (Graph.new_arc f) Graph.empty_graph*)