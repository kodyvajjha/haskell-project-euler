let () =
  let open CCList in
  Z.fac 100 |> Z.to_string |> CCString.to_list >|= CCString.of_char
  >|= int_of_string |> fold_left ( + ) 0 |> print_int
