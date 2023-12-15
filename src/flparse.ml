
type max_node = {
  id: int;
  name: string;
  cap: int;
}

type truc = {
  dep: string;
  arr: string;
  cap: int;
  cost: int;
}

type max = {
  (* departure node + capacity *)
  dep: max_node list;
  (* arrival node + capacity *)
  arr: max_node list;
  (* choice dep to arv *)
  cho: truc list;
}

(* Reads a line with a node. *)
let read_dep acu line i = let f n c = {id=i;name=n;cap=c}::acu
  in Scanf.sscanf line "départ %s %d" f

let read_arr acu line i = let f n c = {id=i;name=n;cap=c}::acu
  in Scanf.sscanf line "arrivé %s %d" f

let read_cho acu line = let f d a ca co = {dep=d;arr=a;cap=ca;cost=co}::acu
  in Scanf.sscanf line "choix %s %s %d %d" f



let read_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop dep_acu arr_acu cho_acu id_acu =
    try let line = input_line infile
      (* Remove leading and trailing spaces. *)
      in let line = String.trim line
      (* Ignore empty lines *)
      in if line = "" then loop dep_acu arr_acu cho_acu id_acu
      (* The first character of a line determines its content : n or e. *)
      else (try match line.[0] with
        | 'd' -> loop (read_dep dep_acu line id_acu) arr_acu cho_acu (id_acu+1)
        | 'a' -> loop dep_acu (read_arr arr_acu line id_acu) cho_acu (id_acu+1)
        | 'c' -> loop dep_acu arr_acu (read_cho cho_acu line) id_acu
        | _ -> failwith "unknow char"
        with e -> Printf.printf "Cannot read line - %s:\n%s\n%!" (Printexc.to_string e) line ; failwith "from_file")
    with End_of_file -> {dep=dep_acu; arr=arr_acu; cho=cho_acu}
  in
  let final = loop [] [] [] 2 in

  close_in infile ;
  final

let print el =
  List.iter (fun n-> Printf.printf "départ: id=%d name=%s capacité=%d\n" n.id n.name n.cap) el.dep;
  List.iter (fun n -> Printf.printf "arrivé: id=%d name=%s capacité=%d\n" n.id n.name n.cap) el.arr;
  List.iter (fun {dep=d;arr=a;cap=ca;cost=co} -> Printf.printf "choix: départ=%s arrivé=%s capacité=%d poids=%d\n" d a ca co) el.cho;
  ()
