open Graph
open Tools

let int_int_of_string i = (0, int_of_string i) 

let init gr = gmap gr int_int_of_string

let get_max =
  let rec loop acu = function
    | {lbl=(a,b);_}::q -> loop (min acu (b-a)) q
    | [] -> acu
  in loop max_int

let rec find_path g a b =
  let rec arc_loop = function
    | [] -> raise Not_found
    | {lbl=(ia, ib);_}::q when ia == ib -> arc_loop q
    | arc::q -> try arc::(find_path g arc.tgt b)
      with Not_found -> arc_loop q
  in
  if a = b then [] else arc_loop (out_arcs g a)

