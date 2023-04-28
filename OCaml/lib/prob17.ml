let ones x =
  match x with
  | 0 -> ""
  | 1 -> "one"
  | 2 -> "two"
  | 3 -> "three"
  | 4 -> "four"
  | 5 -> "five"
  | 6 -> "six"
  | 7 -> "seven"
  | 8 -> "eight"
  | 9 -> "nine"
  | _ -> "!ONES_ERROR!"

let tens x =
  match x with
  | 0 -> "ten"
  | 1 -> "eleven"
  | 2 -> "twelve"
  | 3 -> "thirteen"
  | 4 -> "fourteen"
  | 5 -> "fifteen"
  | 6 -> "sixteen"
  | 7 -> "seventeen"
  | 8 -> "eighteen"
  | 9 -> "nineteen"
  | _ -> "!TENS_ERROR!"

let ten_mults x =
  match x with
  | 0 -> ""
  | 2 -> "twenty"
  | 3 -> "thirty"
  | 4 -> "forty"
  | 5 -> "fifty"
  | 6 -> "sixty"
  | 7 -> "seventy"
  | 8 -> "eighty"
  | 9 -> "ninety"
  | _ -> "!TEN_MULTS_ERROR!"

let digits d =
  let rec dig acc d =
    if d < 10 then
      d :: acc
    else
      dig ((d mod 10) :: acc) (d / 10)
  in
  dig [] d

let pp_hum_num (x : int) : string =
  let digs = digits x in
  match digs with
  | [ x ] -> ones x
  | [ x; y ] ->
    if x = 1 then
      tens y
    else
      ten_mults x ^ " " ^ ones y
  | [ x; 0; 0 ] -> ones x ^ " hundred"
  | [ x; y; z ] ->
    if y = 1 then
      ones x ^ " hundred and " ^ tens z
    else if z = 0 then
      ones x ^ " hundred and " ^ ten_mults y
    else
      ones x ^ " hundred and " ^ ten_mults y ^ " " ^ ones z
  | [ x; _; _; _ ] -> ones x ^ " thousand"
  | _ -> ""

let list_length l = List.fold_left ( + ) 0 l

let () =
  let open CCList in
  1 -- 1000 >|= pp_hum_num >>= String.split_on_char ' ' >|= String.length
  |> list_length |> CCFormat.printf "@[@.%d@]"
