let day = 2

open Core

type entry = { ch : char; lb : int; ub : int; password : string }
[@@deriving sexp]

let line_to_entry (line : string) : entry =
  let parts =
    String.split_on_chars line ~on:[ '-'; ':'; ' ' ]
    |> List.filter ~f:(Fn.non String.is_empty)
  in
  match parts with
  | [ lb; ub; ch; password ] ->
      {
        ch = String.get ch 0;
        lb = Int.of_string lb;
        ub = Int.of_string ub;
        password;
      }
  | _ -> failwith "the fuck"

let is_valid (ent : entry) : bool =
  let matching = Char.equal ent.ch in
  let count = ent.password |> String.to_list |> List.count ~f:matching in
  ent.lb <= count && count <= ent.ub

let is_valid_pt2 (ent : entry) : bool =
  let xor a b = (a || b) && not (a && b) in
  let matching_at (pos : int) =
    Char.equal (String.get ent.password pos) ent.ch
  in
  xor (matching_at (ent.lb - 1)) (matching_at (ent.ub - 1))

let solution_pt1 data =
  List.map ~f:line_to_entry data
  (* |> [%sexp_of: entry list] |> Sexp.to_string_hum |> print_endline; *)
  |> List.filter ~f:is_valid
  |> List.length |> Int.to_string

let solution_pt2 data =
  List.map ~f:line_to_entry data
  |> List.filter ~f:is_valid_pt2
  |> List.length |> Int.to_string
