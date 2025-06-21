let day = 6

open Core
module CharSet = Set.Make (Char)
module CharMap = Map.Make (Char)

let count_unique_chars (group : string list) : int =
  let all_chars = List.bind ~f:String.to_list group in
  let all_alphas = List.filter ~f:Char.is_alpha all_chars in
  CharSet.of_list all_alphas |> Set.length

let split_into_groups (input : string list) : string list list =
  let rec aux (acc : string list list) (wip : string list) (rest : string list)
      =
    match rest with
    | [] -> wip :: acc
    | hd :: tl ->
        if String.is_empty hd then aux (wip :: acc) [] tl
        else aux acc (hd :: wip) tl
  in
  aux [] [] input

let solution_pt1 (data : string list) : string =
  split_into_groups data
  |> List.map ~f:count_unique_chars
  |> List.reduce_exn ~f:( + ) |> Int.to_string

let count_char_appearances (group : string list) : int =
  let group_size = List.length group in
  let all_alphas =
    group |> List.bind ~f:String.to_list |> List.filter ~f:Char.is_alpha
  in
  let rec aux (acc : int CharMap.t) (rest : char list) =
    match rest with
    | [] -> acc
    | hd :: tl ->
        let new_map =
          Map.update acc hd ~f:(function None -> 1 | Some n -> n + 1)
        in
        aux new_map tl
  in
  let acc = aux CharMap.empty all_alphas in
  Map.data acc |> List.count ~f:(Int.equal group_size)

let solution_pt2 (data : string list) =
  split_into_groups data
  |> List.map ~f:count_char_appearances
  |> List.reduce_exn ~f:( + ) |> Int.to_string
