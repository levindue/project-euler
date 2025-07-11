(* Problem 2: Even Fibonacci Numbers *)

let problem2 () =
  let rec aux a b acc =
    if a > 4000000 then acc
    else
      let acc' = if a mod 2 = 0 then acc + a else acc in
      aux b (a + b) acc'
  in
  aux 1 2 0

let () =
  let res2 = problem2 () in
  Printf.printf "%d\n" res2
