let day = 10

open Core

let sort_bag (input : string list) : int list =
  let outlet = 0 in
  let joltages = List.map input ~f:Int.of_string in
  let max_adapter = List.max_elt joltages ~compare |> Option.value_exn in
  let device = max_adapter + 3 in
  List.sort ~compare (outlet :: device :: joltages)

let rec window2 : 'a list -> ('a * 'a) list = function
  | x :: y :: rest -> (x, y) :: window2 (y :: rest)
  | _ -> []

let solution_pt1 (data : string list) : string =
  let pairs = sort_bag data |> window2 in
  let diffs = List.map pairs ~f:(fun (left, right) -> right - left) in
  let groups = List.sort_and_group diffs ~compare in
  let diff_and_count =
    List.map groups ~f:(fun group -> (List.hd_exn group, List.length group))
  in
  let one_and_three_jolts =
    List.filter diff_and_count ~f:(fun (diff, _) -> diff = 1 || diff = 3)
  in
  let pdt =
    List.fold one_and_three_jolts ~init:1 ~f:(fun acc (_, count) -> acc * count)
  in
  pdt |> Int.to_string

let rec ways_starting_from (memo : int Int.Table.t) (sublst : int list) : int =
  match sublst with
  | [] | [ _ ] -> failwith "shouldn't happen"
  | [ _; _ ] -> 1
  | hd :: tl ->
      let candidates : int list =
        List.take_while tl ~f:(fun num -> num - hd <= 3)
      in
      let count_ways_from_candidate (acc : int) (candidate : int) : int =
        let not_candidate = fun rest -> not (phys_equal rest candidate) in
        let eval_from_candidate : unit -> int =
         fun () -> ways_starting_from memo (List.drop_while tl ~f:not_candidate)
        in
        let from_candidate =
          Hashtbl.find_or_add memo candidate ~default:eval_from_candidate
        in
        acc + from_candidate
      in
      List.fold candidates ~init:0 ~f:count_ways_from_candidate

let solution_pt2 (data : string list) : string =
  let bag = sort_bag data in
  ways_starting_from (Int.Table.create ()) bag |> Int.to_string
