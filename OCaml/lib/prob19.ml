(*
   We solve this by implementing Zeller's congruence, which is
    more programmatically-favourable than by using Conway's Doomsday algorithm.
*)
[@@@warning "-32"]

let pp_dow i =
  match i with
  | 0 -> "saturday"
  | 1 -> "sunday"
  | 2 -> "monday"
  | 3 -> "tuesday"
  | 4 -> "wednesday"
  | 5 -> "thursday"
  | 6 -> "friday"
  | _ -> "!ERROR_PP_DOW!"

(**Zeller congruence: [year] is the year mod 100, [century] is floor(full year/100)*)
let zeller ~(day : int) ~(month : int) ~(year : int) ~century =
  let month, year =
    if month < 3 then
      month + 12, year - 1
    else
      month, year
  in
  let month_term =
    Float.floor (13 * (month + 1) / 5 |> float_of_int) |> int_of_float
  in
  let year_term = Float.floor (year / 4 |> float_of_int) |> int_of_float in
  let cent_term = Float.floor (century / 4 |> float_of_int) |> int_of_float in
  (day + month_term + year + year_term + cent_term + (5 * century)) mod 7

let dow ~full_year ~month ~day =
  let year = full_year mod 100 in
  let century = Float.floor (full_year / 100 |> float_of_int) |> int_of_float in
  pp_dow @@ zeller ~day ~month ~year ~century

(* TODO: There might be a bug here somewhere since days of the week
   are printed wrong in January 2000 and February 2000. *)
let first_of_month_dates =
  let open CCList in
  let years = 1901 -- 2000 in
  let months = 1 -- 12 in
  let* y = years in
  let+ m = months in
  CCFormat.printf "@[%d/%d/%d - %s@.@]" y m 1 (dow ~day:1 ~month:m ~full_year:y);
  dow ~day:1 ~month:m ~full_year:y

let answer =
  (List.filter (fun x -> x = "sunday") first_of_month_dates |> List.length) - 1

let () = CCFormat.printf "@.Answer : %d@." answer
