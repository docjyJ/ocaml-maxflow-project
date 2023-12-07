open Gfile

let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 6 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile   : input file containing a graph\n" ^
         "    🟄  source   : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink     : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile  : output file in which the result should be written\n\n"^
         "    🟄  outfile2 : output file in which the result exported for svg should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  and outfile2 = Sys.argv.(5)

  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in
  let outgraph = Suv.graph_flow_to_str (Suv.resolve_flow (Suv.init graph) 0 5) in

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile outgraph in

  export outfile2 outgraph

