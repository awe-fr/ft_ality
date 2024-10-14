type key = {
    key : string;
    action : string
}

type move = {
	name : string;
	move : string list
}

type character = {
    name : string;
    combo : move list
}

type config = {
    key : key list;
    move : move list;
    character : character list
}

let print_key (key : key) =
  let str = (key.key ^ " : " ^ key.action) in
  print_endline str;
  ()

let print_move (move : move) =
  print_string (move.name ^ "  : ");
  let print_move_stack str =
      print_string (str ^ ", ");
  in
  List.iter print_move_stack move.move;
  print_endline "";
  ()

let print_char (character : character) =
  print_string (character.name ^ "  : ");
  print_endline ("");
  List.iter print_move character.combo;
  ()