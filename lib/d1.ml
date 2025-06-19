let day = 1

open Core
module IntSet = Set.Make (Int)

let target_of num = 2020 - num

let rec traverse (set : IntSet.t) (numbers : int list) : int =
  match numbers with
  | [] -> failwith "the fuck?"
  | hd :: tl ->
      let target = target_of hd in
      if Set.mem set hd then hd * target else traverse (Set.add set target) tl

let solution_pt1 (data : string list) : string =
  let numbers = List.map ~f:Int.of_string data in
  let traversed : IntSet.t = IntSet.empty in
  let found = traverse traversed numbers in
  found |> Int.to_string

let rec find_pairs (lst : 'a list) : ('a * 'a) list =
  match lst with
  | [] | [ _ ] -> []
  | hd :: tl -> List.map ~f:(fun o -> (hd, o)) tl @ find_pairs tl

let target_of_pair ((a, b) : int * int) = 2020 - a - b

let try_find_soln (set : IntSet.t) ((a, b) : int * int) : int option =
  let target = target_of_pair (a, b) in
  if Set.mem set target then Some (a * b * target) else None

let solution_pt2 (data : string list) : string =
  let numbers = List.map ~f:Int.of_string data in
  let number_set = IntSet.of_list numbers in
  let pairs = find_pairs numbers in
  List.find_map_exn ~f:(try_find_soln number_set) pairs |> Int.to_string
