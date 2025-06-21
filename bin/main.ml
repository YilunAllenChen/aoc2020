open Aoc
open Core

let () =
  let input_data = data_for_day Wip.day in
  print_endline "\n====pt1=====";
  (try Wip.solution_pt1 input_data |> print_endline
   with exn -> Printf.printf "Caught: %s\n" (Exn.to_string exn));

  print_endline "\n====pt2=====";
  try Wip.solution_pt2 input_data |> print_endline
  with exn -> Printf.printf "Caught: %s\n" (Exn.to_string exn)
