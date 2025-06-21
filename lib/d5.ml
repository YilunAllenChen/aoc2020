let day = 5

open Core

let parse_char : char -> int = function
  | 'B' | 'R' -> 1
  | 'F' | 'L' -> 0
  | _ -> failwith "the fuck"

let id_to_num (line : string) : int =
  let char_lst = String.to_list line in
  let rec aux (acc : int) (rest : char list) =
    match rest with [] -> acc | hd :: tl -> aux ((acc * 2) + parse_char hd) tl
  in
  aux 0 char_lst

let solution_pt1 (data : string list) : string =
  let maybe_max =
    data |> List.map ~f:id_to_num |> List.max_elt ~compare:Int.compare
  in
  match maybe_max with Some max -> Int.to_string max | None -> "the fuck"

let rec window2 : 'a list -> ('a * 'a) list = function
  | x :: y :: rest -> (x, y) :: window2 (y :: rest)
  | _ -> []

let solution_pt2 (data : string list) : string =
  let sorted = List.map ~f:id_to_num data |> List.sort ~compare:Int.compare in
  let pairs = window2 sorted in
  let maybe_my_seat = List.find pairs ~f:(fun (fst, snd) -> snd - fst = 2) in
  match maybe_my_seat with
  | Some (_, my_right) -> my_right - 1 |> Int.to_string
  | None -> failwith "sadge"
