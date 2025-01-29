exception Wrong_number of string

(* Print character move *)

let selected_char (character : Type.character list) (key : Type.key list) (move : Type.move list) num =
  try
    let car = List.nth character num in
    print_endline ("");
    print_string ("You have selected ");
    Type.print_char car;
    Type.print_move_list move;
    Type.print_key key;
    print_endline ("------------Input------------");
    Automaton.start_automaton key car move;
    ()
  with _ ->
    raise (Wrong_number ((string_of_int (num + 1)) ^ " is not affialied to any character"))

(* Character selection *)

let rec select_character (config : Type.config) =
  let event = Tsdl.Sdl.Event.create () in
  Tsdl.Sdl.wait_event (Some event) |> ignore;
  let event_type = Tsdl.Sdl.Event.enum (Tsdl.Sdl.Event.get event Tsdl.Sdl.Event.typ) in
  
  match event_type with
  | `Quit ->
      exit 0
  | `Key_down ->
      let keycode = Tsdl.Sdl.Event.get event Tsdl.Sdl.Event.keyboard_keycode in
      let num = int_of_string (Tsdl.Sdl.get_key_name keycode) in
      if num = 0 then
        raise (Wrong_number "Cannot select 0")
      else
        selected_char config.character config.key config.move (num - 1)
  | _ -> 
    select_character config

(* Basic init of SDL *)

let init (config : Type.config) =
  match Tsdl.Sdl.init Tsdl.Sdl.Init.video with
  | Error (`Msg e) -> Tsdl.Sdl.log "Error failed init SDL: %s" e
  | Ok () ->
    let window = Tsdl.Sdl.create_window ~w:0 ~h:0 "ft_ality" Tsdl.Sdl.Window.(borderless + input_focus) in
    match window with
    | Error (`Msg e) -> Tsdl.Sdl.log "Error window creation: %s" e
    | Ok window ->
      let renderer = Tsdl.Sdl.create_renderer window ~flags:Tsdl.Sdl.Renderer.(accelerated + presentvsync) in
      match renderer with
      | Error (`Msg e) -> Tsdl.Sdl.log "Error renderer creation: %s" e
      | Ok renderer ->
        match Tsdl.Sdl.render_clear renderer with
        | Error (`Msg e) -> Tsdl.Sdl.log "Error when cleaning window: %s" e
        | Ok () ->
          Tsdl.Sdl.render_present renderer;

          Type.print_character config.character;
          select_character config;

          Tsdl.Sdl.destroy_renderer renderer;
          Tsdl.Sdl.destroy_window window;
          Tsdl.Sdl.quit ()