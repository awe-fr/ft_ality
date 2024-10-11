exception Wrong_content of string

type key = {
    key : string;
    action : string
}

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

let print_key (key : key) =
    let str = (key.key ^ " : " ^ key.action) in
    print_endline str;
    ()

let rec key_parse file =
    let line = input_line file in
    let line = off_space line in
    if line <> "END" then
        let splitted = String.split_on_char '=' line in
        let ac = List.nth (String.split_on_char '\"' (List.nth splitted 1)) 1 in
        let key : key = {key = List.nth splitted 0; action = ac} in
        key :: key_parse file
    else
        []

let move_parse file =
    let cat = input_line file |> off_space in
    if cat = "BASIC_MOVES" then
        ()
    else if cat = "CHARACTER_MOVES" then
        ()
    else
        raise (Wrong_content "Wrong categories")

let start_parsing path =
    let file = open_in path in
    let cat = input_line file |> off_space in
    if cat = "KEY_MAPPING" then
        let lst = key_parse file in
        List.iter print_key lst;
        let _ = input_line file in
        let cat2 = input_line file |> off_space in
        if cat2 = "MOVE_LIST" then
            let _ = move_parse file in
            ()
        else
            ()
    else if cat = "MOVE_LIST" then
        (* let _ = move_parse file in *)
        ()
    else
        raise (Wrong_content "Wrong categories")