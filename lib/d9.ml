let day = 9

open Core
module IntSet = Set.Make (Int)

type preamble = { ingredients : int list; targets : IntSet.t }
[@@deriving sexp_of]

let append (lst : 'a list) (ele : 'a) : 'a list =
  List.rev_append (List.rev lst) [ ele ]

let find_pairs (lst : 'a list) : ('a * 'a) list =
  let rec aux (acc : ('a * 'a) list) (rest : 'a list) =
    match rest with
    | [] -> acc
    | hd :: tl -> aux (acc @ List.map tl ~f:(fun ele -> (hd, ele))) tl
  in
  aux [] lst

let build_preamble (ingredients : int list) : preamble =
  let pairs = find_pairs ingredients in
  let targets_lst = pairs |> List.map ~f:(fun (left, right) -> left + right) in
  let targets = IntSet.of_list targets_lst in
  { ingredients; targets }

let update (pm : preamble) (num : int) : preamble =
  match pm.ingredients with
  | [] -> failwith "ingredients can't be empty"
  | _ :: tl -> build_preamble (append tl num)

let is_valid_next (pm : preamble) (next : int) : bool = Set.mem pm.targets next

let rec find_broken (pm : preamble) (seq : int list) : int =
  match seq with
  | [] -> failwith "nothing broken"
  | hd :: tl ->
      if is_valid_next pm hd then find_broken (update pm hd) tl else hd

let solution_pt1 (data : string list) : string =
  let inputs = List.map data ~f:Int.of_string in
  let init_ingredients, rest = List.split_n inputs 25 in
  let preamble = build_preamble init_ingredients in
  find_broken preamble rest |> Int.to_string

let find_seq (lst : int list) (target : int) : int * int =
  let rec aux (wip : int list) (rest : int list) : int * int =
    match Int.compare (List.fold wip ~init:0 ~f:( + )) target with
    | x when x = 0 ->
        ( List.max_elt ~compare:Int.compare wip |> Option.value_exn,
          List.min_elt ~compare:Int.compare wip |> Option.value_exn )
    | x when x < 0 -> (
        match rest with
        | [] -> failwith "exhausted list"
        | hd :: tl -> aux (append wip hd) tl)
    | x when x > 0 -> (
        match wip with [] -> failwith "impossible" | _ :: tl -> aux tl rest)
    | _ -> failwith "wut"
  in
  aux [] lst

let solution_pt2 (data : string list) : string =
  let inputs = List.map data ~f:Int.of_string in
  let init_ingredients, rest = List.split_n inputs 25 in
  let preamble = build_preamble init_ingredients in
  let broken_num = find_broken preamble rest in
  let left, right = find_seq inputs broken_num in
  left + right |> Int.to_string
