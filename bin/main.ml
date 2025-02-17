(* Just main func *)

let () =

  let help () = 
    print_endline ("usage: ft_ality [-h] [--help] config_file [-d]\n\n");
    print_endline ("positional arguments:\n");
    print_endline ("    config_file     json description of the machine\n\n");
    print_endline ("optional arguments:\n");
    print_endline ("    -h, --help      show this help message and exit\n");
    print_endline ("    -d              turn on the debug mode\n");
    ()
  in

  let rec check_help ac index =
    if index >= ac then
      -1
    else
      let cmd = Sys.argv.(index) in
      if cmd = "--help" || cmd = "-h" then begin
        help ();
        1
      end else begin
        check_help ac (index + 1)
      end
  in

  let check_debug tc =
    if tc = "-d" then begin
      Debug.debug_mode := 1
    end else begin
      Debug.debug_mode := 0
    end
  in

  let ac = Array.length Sys.argv in
  let is = check_help ac 1 in
  if is = -1 then
    if ac = 2 || ac = 3 then
      try
        if ac = 3 then begin
          let _ = check_debug Sys.argv.(2) in
          let config = Parser.start_parsing Sys.argv.(1) in
          Sdl_init.init config
        end else begin
          let config = Parser.start_parsing Sys.argv.(1) in
          Sdl_init.init config
        end
      with
      | Sdl_init.Wrong_number msg ->
        print_endline ("Error : " ^ msg);
      | Parser.Wrong_content msg ->
        print_endline ("Error : " ^ msg);
      | _ ->
        print_endline ("Error");
    else
      print_endline ("Usage : ./ft_ality [-h] [--help] config_file [-d]")
  else
    ()
