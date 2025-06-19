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

let is_on_slope (xmax : int) (loc : location) : bool =
  let padding_multiplier = loc.y * 3 / xmax in
  let padded_x = loc.x + (xmax * padding_multiplier) in

  match loc.y with 0 -> false | y -> Int.equal padded_x (y * 3)

let solution_pt1 (data : string list) : string =
  let xmax = List.hd_exn data |> String.length in
  let all_tree_locs = gather_tree_locations data in
  let trees_on_slope = all_tree_locs |> List.filter ~f:(is_on_slope xmax) in

  (* trees_on_slope |> [%sexp_of: location list] |> Sexp.to_string_hum *)
  trees_on_slope |> List.length |> Int.to_string

let solution_pt2 _ = ""
