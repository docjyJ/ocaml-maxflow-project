open Gfile
open Tools


let dfs graph src trg =

    let rec check_arc arc =
        if arc.tgt=trg then arc::[]
        else if arc.lbl.max!=arc.lbl.value then explore_node arc.tgt
             else []
    in

    let rec explore_list_arc list =
        match list with
        | [] -> []
        | x::rest -> if (check_arc x)!=[] then  x::(explore_node x.tgt) else explore_list_arc rest
    in


    let rec explore_node node =
        let outa = out_arcs gr node in

        match (outa) with
        | (_) -> []
        | ([]) -> []
        | (x::rest) -> explore_list_arc
    in

    explore_node src;
