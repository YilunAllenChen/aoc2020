let day = 3

open Core

type location = { y : int; x : int } [@@deriving sexp]
type cell = Tree | Space

let parse_cell : char -> cell = function
  | '#' -> Tree
  | '.' -> Space
  | _ -> failwith "garbage"

let parse_input (text : string list) : cell list list =
  let parse_line line = line |> String.to_list |> List.map ~f:parse_cell in
  text |> List.map ~f:parse_line

let gather_tree_locations (text : string list) : location list =
  let cells = parse_input text in
  let acc_in_line (y : int) (line : cell list) : location list =
    List.filter_mapi line ~f:(fun x cell ->
        match cell with Tree -> Some { x; y } | Space -> None)
  in
  List.mapi cells ~f:acc_in_line |> List.bind ~f:Fn.id

let is_on_slope (xmax : int) (xmult : int) (ymult : int) (loc : location) : bool
    =
  let padding_multiplier = loc.y * ymult / xmult / xmax in
  let padded_x = loc.x + (xmax * padding_multiplier) in
  Int.equal (padded_x * xmult) (loc.y * ymult)

let solution_pt1 (data : string list) : string =
  let xmax = List.hd_exn data |> String.length in
  let all_tree_locs = gather_tree_locations data in
  let trees_on_slope = all_tree_locs |> List.filter ~f:(is_on_slope xmax 1 3) in
  trees_on_slope |> List.length |> Int.to_string

let solution_pt2 (data : string list) : string =
  let xmax = List.hd_exn data |> String.length in
  let all_tree_locs = gather_tree_locations data in

  let slopes = [ (1, 1); (1, 3); (1, 5); (1, 7); (2, 1) ] in
  let product =
    List.fold slopes ~init:1 ~f:(fun acc (xmult, ymult) ->
        acc
        * (all_tree_locs
          |> List.filter ~f:(is_on_slope xmax xmult ymult)
          |> List.length))
  in
  product |> Int.to_string
