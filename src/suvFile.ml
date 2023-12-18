open Graph
open Suv

type path = string

type node_dictionary = (id * string) list

let find_id test nodes = Option.get (List.find_map (fun (i, name) -> if test = name then Some(i) else None) nodes)
let find_name test nodes = Option.get (List.find_map (fun (i, name) -> if test = i then Some(name) else None) nodes)

let build_arc gr src tgt cap = new_arc gr {src=src;tgt=tgt;lbl=flow_of_string cap}

let new_dep_node (nodes,i,gr) n c = ((i,n)::nodes), (i+1), (build_arc (new_node gr i) 0 i c)
let new_arr_node (nodes,i,gr) n c = ((i,n)::nodes), (i+1), (build_arc (new_node gr i) i 1 c)
let new_choice_arc (nodes,i,gr) src tgt cap = nodes, i, (build_arc gr (find_id src nodes) (find_id tgt nodes) cap)



let read_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop acu =
    try let line = String.trim (input_line infile)
      in loop (
      if line = "" then acu
      else try match line.[0] with
        | 'D' -> Scanf.sscanf line "D %s %s" (new_dep_node acu)
        | 'A' -> Scanf.sscanf line "A %s %s" (new_arr_node acu)
        | 'I' -> Scanf.sscanf line "I %s %s %s" (new_choice_arc acu)
        | '%' -> acu
        | _ -> failwith "unknow char"
        with e -> Printf.printf "Cannot read line - %s:\n%s\n%!" (Printexc.to_string e) line ; failwith "from_file"
        )
    with End_of_file -> acu
  in let nodes, _, final_graph = loop ([(1,"Arrivé");(0,"Départ")], 2, new_node (new_node empty_graph 0) 1)
  in close_in infile ; (nodes, final_graph)



let export path dict g =
  (* Open a write-file. *)
  let ff = open_out path in
  let loop arc = Printf.fprintf ff "    %s -> %s [xlabel = \"%s\", style = \"%s\"];\n"
  (find_name arc.src dict) (find_name arc.tgt dict) (string_of_int arc.lbl.use) (if arc.lbl.use = 0 then "dotted" else if arc.lbl.spc = 0 then "bold" else "solid" )
  in

  (* Write in this file. *)
  Printf.fprintf ff "digraph finite_state_machine {\n";
  Printf.fprintf ff "    graph [pad=\"0.5\", nodesep=\"1\", ranksep=\"2\"];\n";
  Printf.fprintf ff "    splines=false;\n";
  Printf.fprintf ff "    fontname=\"Helvetica,Arial,sans-serif\";\n";
  Printf.fprintf ff "    node[fontname=\"Helvetica,Arial,sans-serif\"];\n";
  Printf.fprintf ff "    edge[fontname=\"Helvetica,Arial,sans-serif\"];\n";
  Printf.fprintf ff "    rankdir=LR;\n";
  Printf.fprintf ff "    node [shape = box];\n";

  (* Write all arcs *)
  Tools.arc_iter loop g;

  Printf.fprintf ff "}\n" ;

  close_out ff ;
  ()

let export_all dir dict = List.iteri (fun i -> export (dir^"/"^(string_of_int i)^".txt") dict)