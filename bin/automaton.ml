type state = {
  state : string;
  full_combos : Type.move list;
  key : Type.key list;
  potential_combo : Type.move list;
  combo_index : int
}




let potential_combo_calculator (state : state) input =

  let is_here input index (combo : Type.move) =
    try
      let texte = List.nth combo.move index in
      if input = texte then
        true
      else
        false
    with _ ->
      false
  in

  let rec create_new_combo_list input index (combo_list : Type.move list) =
    match combo_list with
    | [] -> []
    | x :: xs ->
      try
        let texte = List.nth x.move index in
        if input = texte then
          x :: create_new_combo_list input index xs
        else
          create_new_combo_list input index xs
      with _ ->
        create_new_combo_list input index xs
  in

  if List.exists (is_here input state.combo_index) state.potential_combo = true then
    create_new_combo_list input state.combo_index state.potential_combo
  else
    []





let interpreter (state : state) input =
  let new_state = Type.print_assosiate state.key input in
  if new_state <> "none" then begin
    let new_potential_combo = potential_combo_calculator state new_state in
    List.iter Type.print_move new_potential_combo;
    if state.state = "neutral" then
      let state : state = {state = new_state; full_combos = state.full_combos; key = state.key; potential_combo = state.potential_combo; combo_index = (state.combo_index + 1)} in
      print_endline state.state;
      state
    else
      let state : state = {state = (state.state ^ " " ^ new_state); full_combos = state.full_combos; key = state.key; potential_combo = state.potential_combo; combo_index = (state.combo_index + 1)} in
      print_endline state.state;
      state
  end else begin
    state
  end






let rec wait_for_input (state : state) =
  let event = Tsdl.Sdl.Event.create () in
  Tsdl.Sdl.wait_event (Some event) |> ignore;
  let event_type = Tsdl.Sdl.Event.enum (Tsdl.Sdl.Event.get event Tsdl.Sdl.Event.typ) in
  
  match event_type with
  | `Quit ->
      exit 0
  | `Key_down ->
      let keycode = Tsdl.Sdl.Event.get event Tsdl.Sdl.Event.keyboard_keycode in
      let input = Tsdl.Sdl.get_key_name keycode in
      let state = interpreter state input in
      wait_for_input state;
      ()
  | _ ->
    wait_for_input state

let start_automaton (key : Type.key list) (character : Type.character) (move : Type.move list) =
  let combos = character.combo @ move in
  let state : state = {state = "neutral"; full_combos = combos; key = key; potential_combo = combos; combo_index = 0} in
  wait_for_input state;
