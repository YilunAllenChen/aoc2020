let day = 11

open Core

module Loc = struct
  type t = int * int [@@deriving compare, hash, sexp]
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

let neighbor_of (area : area) ((x, y) : int * int) : (int * int) list =
  let ymax, xmax = area.shape in
  let raw = raw_neighbor_of (x, y) in
  let bound ((x, y) : int * int) : bool =
    Int.between x ~low:0 ~high:xmax && Int.between y ~low:0 ~high:ymax
  in
  List.filter raw ~f:bound

let immediate_occupied_neighbors (area : area) ((x, y) : int * int) :
    (int * int) list =
  let neighbors = neighbor_of area (x, y) in
  List.filter neighbors ~f:(Set.mem area.occupied)

let count_neighbors (area : area) ~neighbors_of ((x, y) : int * int) : int =
  neighbors_of (x, y) |> List.count ~f:(Set.mem area.occupied)

let step ~neighbors_of ~leave_threshold (area : area) : area * stats =
  let count = count_neighbors area ~neighbors_of in

  let seated, still_empty =
    Set.to_list area.empty |> List.partition_tf ~f:(fun loc -> count loc = 0)
    |> fun (yes, no) -> (LocSet.of_list yes, LocSet.of_list no)
  in
  let occupied = Set.union area.occupied seated in
  let wip = { area with occupied; empty = still_empty } in

  let left, still_occupied =
    Set.to_list wip.occupied
    |> List.partition_tf ~f:(fun loc -> count loc >= leave_threshold)
    |> fun (yes, no) -> (LocSet.of_list yes, LocSet.of_list no)
  in
  let empty = Set.union wip.empty left in
  let result = { wip with occupied = still_occupied; empty } in
  let stats =
    {
      newly_seated = Set.length result.occupied - Set.length area.occupied;
      newly_left = Set.length result.empty - Set.length area.empty;
    }
  in
  (result, stats)

let rec stablize (area : area) (stats : stats option)
    ~(step : area -> area * stats) : area =
  match stats with
  | Some { newly_seated = 0; newly_left = 0 } -> area
  | _ ->
      let new_area, new_stats = step area in
      stablize new_area (Some new_stats) ~step

let solution_pt1 (data : string list) : string =
  let area = load_area data in
  let neighbors_of = immediate_occupied_neighbors area in
  let stablized =
    stablize area None ~step:(step ~neighbors_of ~leave_threshold:4)
  in
  Set.length stablized.occupied |> Int.to_string

let directions =
  [ (-1, -1); (-1, 0); (-1, 1); (0, -1); (0, 1); (1, -1); (1, 0); (1, 1) ]

let visible_neighbor_cache : (int * int, (int * int) list) Hashtbl.t =
  Hashtbl.create (module Loc)

let visible_neighbors (area : area) ((x, y) : int * int) : (int * int) list =
  Hashtbl.find_or_add visible_neighbor_cache (x, y) ~default:(fun () ->
      let ymax, xmax = area.shape in
      let is_seat = Set.mem area.empty in
      let rec first_seat_in_dir (dx, dy) (x, y) =
        let x', y' = (x + dx, y + dy) in
        if x' < 0 || x' >= xmax || y' < 0 || y' >= ymax then None
        else if is_seat (x', y') then Some (x', y')
        else first_seat_in_dir (dx, dy) (x', y')
      in
      List.filter_map directions ~f:(fun dir -> first_seat_in_dir dir (x, y)))

let solution_pt2 (data : string list) : string =
  let area = load_area data in
  let neighbors_of = visible_neighbors area in
  let stabilized =
    stablize area None ~step:(step ~neighbors_of ~leave_threshold:5)
  in
  Set.length stabilized.occupied |> Int.to_string
