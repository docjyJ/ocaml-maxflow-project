let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 3 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile   : input file containing a graph\n" ^
         "    🟄  out_dir  : output directory in which the result exported for svg should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and out_dir = Sys.argv.(2)
  in

  (* Open file *)
  let d, g = SuvFile.read_file infile
  in let g_list = Suv.resolve_flow_with_step_list g (0,1)
  in SuvFile.export_all out_dir d g_list