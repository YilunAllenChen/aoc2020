let day = 11

open Core

module Loc = struct
  type t = int * int [@@deriving compare, sexp]
end

module LocSet = Set.Make (Loc)

type area = { occupied : LocSet.t; empty : LocSet.t; shape : int * int }
[@@deriving sexp]

let load_area (input : string list) : area =
  let seat_locs =
    List.mapi input ~f:(fun y row ->
        let cells = String.to_list row in
        List.filter_mapi cells ~f:(fun x cell ->
            match cell with 'L' -> Some (x, y) | _ -> None))
    |> List.bind ~f:Fn.id
  in
  let ymax = List.length input in
  let xmax = String.length (List.hd_exn input) in
  {
    occupied = LocSet.empty;
    empty = LocSet.of_list seat_locs;
    shape = (ymax, xmax);
  }

type stats = { newly_seated : int; newly_left : int } [@@deriving sexp]

let neighbor_cache : (int * int, (int * int) list) Hashtbl.t =
  Hashtbl.create
    (module struct
      type t = int * int [@@deriving compare, hash, sexp]
    end)

let raw_neighbor_of (coord : int * int) : (int * int) list =
  Hashtbl.find_or_add neighbor_cache coord ~default:(fun () ->
      let x, y = coord in
      [
        (x - 1, y - 1);
        (x - 1, y);
        (x - 1, y + 1);
        (x, y - 1);
        (x, y + 1);
        (x + 1, y - 1);
        (x + 1, y);
        (x + 1, y + 1);
      ])

let neighbor_of ((x, y) : int * int) (area : area) : (int * int) list =
  let ymax, xmax = area.shape in
  let raw = raw_neighbor_of (x, y) in
  let bound ((x, y) : int * int) : bool =
    Int.between x ~low:0 ~high:xmax && Int.between y ~low:0 ~high:ymax
  in
  List.filter raw ~f:bound

let count_occupied_neighbors ((x, y) : int * int) (area : area) : int =
  let neighbors = neighbor_of (x, y) area in
  let occupied_neighboars = List.filter neighbors ~f:(Set.mem area.occupied) in
  List.length occupied_neighboars

let should_populate ((x, y) : int * int) (area : area) : bool =
  count_occupied_neighbors (x, y) area = 0

let should_leave ((x, y) : int * int) (area : area) : bool =
  count_occupied_neighbors (x, y) area >= 4

let step (area : area) : area * stats =
  let wip = area in

  let occ_after_popu, emp_after_popu =
    wip.empty |> Set.to_list
    |> List.partition_tf ~f:(fun loc -> should_populate loc wip)
    |> fun (x, y) -> (LocSet.of_list x, LocSet.of_list y)
  in
  let new_occupied = Set.union wip.occupied occ_after_popu in
  let wip = { wip with occupied = new_occupied; empty = emp_after_popu } in

  let emp_after_leave, occ_after_leave =
    wip.occupied |> Set.to_list
    |> List.partition_tf ~f:(fun loc -> should_leave loc wip)
    |> fun (x, y) -> (LocSet.of_list x, LocSet.of_list y)
  in
  let new_empty = Set.union wip.empty emp_after_leave in
  let wip = { wip with occupied = occ_after_leave; empty = new_empty } in
  let stats =
    {
      newly_seated = Set.length wip.occupied - Set.length area.occupied;
      newly_left = Set.length wip.empty - Set.length area.empty;
    }
  in
  (wip, stats)

let rec stablize (area : area) (stats : stats option) : area =
  match stats with
  | Some stats when stats.newly_left = 0 && stats.newly_seated = 0 -> area
  | _ ->
      let new_area, stats = step area in
      stablize new_area (Some stats)

let solution_pt1 (data : string list) : string =
  let area = load_area data in
  let stablized = stablize area None in
  Set.length stablized.occupied |> Int.to_string

let solution_pt2 (data : string list) : string =
  let _ = data in
  ""
