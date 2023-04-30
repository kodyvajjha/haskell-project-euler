[@@@warning "-32"]

let divisors n =
  let ans = ref [] in
  for i = 1 to n / 2 do
    if n mod i = 0 then
      ans := !ans @ [ i ]
    else
      ()
  done;
  !ans

let sigma n = CCList.fold_left ( + ) 0 (divisors n)

let pp_list fpf l =
  CCFormat.fprintf fpf "@[%a@]" CCFormat.(list ~sep:(return ";") int) l

let pp_sigma n =
  CCFormat.printf "@[@.Divisors of %d : [%a]@.Sum of divisors of %d : %d @]" n
    pp_list (divisors n) n (sigma n)

let amicables ~upto =
  let ans = ref [] in
  for i = 1 to upto do
    let j = sigma i in
    if sigma j = i && i <> j then
      ans := !ans @ [ i, j ]
    else
      ()
  done;
  !ans

let pp_amicables =
  CCFormat.printf "[%a]"
    CCFormat.(list ~sep:(return ";") (pair ~sep:(return ",") int int))

let answer =
  let am = amicables ~upto:10000 in
  (CCList.fold_left ( + ) 0 @@ List.map (fun (a, b) -> a + b) am) / 2

let () = print_int answer
