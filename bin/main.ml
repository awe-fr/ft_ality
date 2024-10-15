let () =
  try
    let config = Parser.start_parsing "./configs/mk1.cfg" in
    let _ = Sdl_init.init config in
    ()
  with
  | Sdl_init.Wrong_number msg ->
    print_endline ("Error : " ^ msg);
  | _ ->
    print_endline ("Error");
