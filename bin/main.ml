let () =
  let ac = Array.length Sys.argv in
  if ac = 2 then
    try
      let config = Parser.start_parsing Sys.argv.(1) in
      Sdl_init.init config
    with
    | Sdl_init.Wrong_number msg ->
      print_endline ("Error : " ^ msg);
    | _ ->
      print_endline ("Error");
  else
    print_endline ("Usage : ./ft_ality config_file")
