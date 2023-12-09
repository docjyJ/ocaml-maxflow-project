open Graph

let clone_nodes g = n_fold g new_node empty_graph

let label_map g f =
  let loop g a = new_arc g {a with lbl=f a.lbl}
  in e_fold g loop (clone_nodes g)

let add_arc g id1 id2 n =
  match find_arc g id1 id2 with
  | None -> new_arc g {src=id1; tgt=id2; lbl=n}
  | Some x -> new_arc g {src=id1; tgt=id2; lbl=(n+x.lbl)}

let find_lbl_arc g id1 id2 =
  match find_arc g id1 id2 with
  | None -> raise(Graph_error("Arc from " ^ string_of_int id1 ^ " to "  ^ string_of_int id2 ^ " not found in the graph."))
  | Some x -> x.lbl

let unify_arc g_new g_old =
  let loop g a = new_arc g {a with lbl=find_lbl_arc g_new a.src a.tgt}
  in e_fold g_old loop (clone_nodes g_old)