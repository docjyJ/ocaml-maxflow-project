open Graph

let clone_nodes gr = Graph.n_fold gr Graph.new_node Graph.empty_graph

let gmap gr f = Graph.e_fold gr (fun g a -> Graph.new_arc g {src=a.src;tgt=a.tgt;lbl=f a.lbl}) (clone_nodes gr)

let add_arc g id1 id2 n =
match Graph.find_arc g id1 id2 with
| None -> Graph.new_arc g {src =id1; tgt=id2; lbl=n}
| Some x -> Graph.new_arc g {src =id1; tgt=id2; lbl=(n+x.lbl)};

