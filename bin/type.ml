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

let print_key (key : key list) =
  let print_key_stack (key : key) =
    let str = (key.key ^ " : " ^ key.action) in
    print_endline (str)
  in
  print_endline ("-------------key-------------");
  List.iter print_key_stack key;
  ()

let print_move_list (move : move list) =
  let print_move_list_func (move : move)=
    print_string ("[" ^ move.name ^ "] : ");
    let print_move_stack str =
        print_string (str ^ ", ");
    in
    List.iter print_move_stack move.move;
    print_endline "";
    ()
  in
  print_endline ("------------Combo------------");
  List.iter print_move_list_func move ;
  ()

let print_move (move : move) =
  print_string ("[" ^ move.name ^ "] : ");
  let print_move_stack str =
      print_string (str ^ ", ");
  in
  List.iter print_move_stack move.move;
  print_endline "";
  ()

let print_char (character : character) =
  print_endline (character.name);
  print_endline ("--------Special Combo--------");
  List.iter print_move character.combo;
  ()

let print_character (character : character list) =
  let rec print_char_stack (character : character list) num =
    match character with
    | [] -> ()
    | x :: xs -> print_endline ((string_of_int num) ^ " : " ^ x.name); print_char_stack xs (num + 1)
  in
  print_char_stack character 1

let rec get_assosiate (key : key list) input =
  match key with
  | [] -> "none"
  | x :: xs -> 
    if x.key = input then begin
      x.action 
    end else begin 
      get_assosiate xs input
    end