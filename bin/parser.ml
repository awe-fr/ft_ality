exception Wrong_content of string

(* Get rid of space and tab *)

let off_space file =
  let index = 0 in
  let rec outside file index = 
    let rec inside file index =
      if index >= String.length file then
        ""
      else if String.get file index = '"' then
        let ne = String.get file index in
        let n = String.make 1 ne in
        n ^ outside file (index + 1)
      else
        let ne = String.get file index in
        let n = String.make 1 ne in
        n ^ inside file (index + 1)
    in
    if index >= String.length file then
      ""
    else if String.get file index = '"' then
      let ne = String.get file index in
      let n = String.make 1 ne in
      n ^ inside file (index + 1)
    else if String.get file index = ' ' then
      outside file (index + 1)
    else if String.get file index = '\t' then
      outside file (index + 1)
    else
      let ne = String.get file index in
      let n = String.make 1 ne in
      n ^ outside file (index + 1)
  in
  outside file index

(* Parse KEY_MAPPING section *)

let rec key_parse file =
    let line = input_line file in
    let line = off_space line in
    if line <> "END" then
        let splitted = String.split_on_char '=' line in
        let ac = List.nth (String.split_on_char '\"' (List.nth splitted 1)) 1 in
        let key : Type.key = {key = List.nth splitted 0; action = ac} in
        if key.action = "" then
          raise (Wrong_content "Not closed")
        else
          key :: key_parse file
    else
        []

(* Check if the move is in the list *)

let rec check_there name (combo : Type.key list) =
  match combo with
  | [] -> false
  | x :: xs -> if name = x.action then true else check_there name xs

(* Get the combo list *)

let rec get_lst lst lste =
    match lst with
    | [] -> []
    | x :: xs -> let splitted = String.split_on_char '"' x in let car = List.nth splitted 1 in if check_there car lste = false then raise(Wrong_content "Move not in the list") else car :: get_lst xs lste

(* BASIC_MOVES parsing *)

let rec basic_move_parse file lste =
    let line = input_line file in
    let line = off_space line in
    if line <> "END" then
        let splitted = String.split_on_char '=' line in
        let name = List.nth splitted 0 in
        let namesplitted = String.split_on_char '"' name in
        let name = List.nth namesplitted 1 in
        let lst = String.split_on_char ',' (List.nth splitted 1) in
        let lst = get_lst lst lste in
        let move : Type.move = {name = name; move = lst} in
        if move.name = "" then
          raise (Wrong_content "Not closed")
        else
          move :: basic_move_parse file lste
    else
        []

(* Parsing charracter *)

let rec parse_char file lst =
    let line = input_line file in
    let line = off_space line in
    if line <> "END" then
        let splitted = String.split_on_char '"' line in
        let name = List.nth splitted 1 in
        let combo = basic_move_parse file lst in
        let _ = input_line file in
        let character : Type.character = {name = name; combo = combo} in
        if character.name = "" then
          raise (Wrong_content "Not closed")
        else
          character :: parse_char file lst
    else
        []

(* Start MOVE_LIST parsing *)

let move_parse file (lst : Type.key list) =
    let cat = input_line file |> off_space in
    if cat = "BASIC_MOVES" then
        let move = basic_move_parse file lst in
        let _ = input_line file in
        let cat2 = input_line file |> off_space in
        if cat2 = "CHARACTER_MOVES" then
            let characters = parse_char file lst in
            let config : Type.config = {key = lst; move = move; character = characters} in
            config
        else
            raise (Wrong_content "Wrong categories")
    else
        raise (Wrong_content "Wrong categories")

(* Start parsing *)

let start_parsing path =
    let file = open_in path in
    let cat = input_line file |> off_space in
    if cat = "KEY_MAPPING" then
        let lst = key_parse file in
        let _ = input_line file in
        let cat2 = input_line file |> off_space in
        if cat2 = "MOVE_LIST" then
            let conf = move_parse file lst in
            conf
        else
            raise (Wrong_content "Wrong categories")
    else
        raise (Wrong_content "Wrong categories")