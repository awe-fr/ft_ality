type state = {
  state : string;
  full_combos : Type.move list;
  key : Type.key list;
  potential_combo : Type.move list
}

let start_automaton (key : Type.key list) (character : Type.character) (move : Type.move list) =
  let combos = character.combo @ move in
  let _ : state = {state = "neutral"; full_combos = combos; key = key; potential_combo = combos} in
  print_endline ("test")
