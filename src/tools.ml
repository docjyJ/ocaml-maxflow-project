open Graph

let clone_nodes gr = Graph.n_fold gr Graph.new_node Graph.empty_graph

let gmap gr f = Graph.e_fold gr (fun g a -> Graph.new_arc g {src=a.src;tgt=a.tgt;lbl=f a.lbl}) (clone_nodes gr)