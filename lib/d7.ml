let day = 7

open Core

let outer_re = Re.Pcre.regexp "^([a-z ]+) bags contain"
let inner_re = Re.Pcre.regexp "([0-9]+) ([a-z ]+) bag"

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

type rule = { color : string; can_contain : int StringMap.t } [@@deriving sexp]

let parse_line (line : string) : rule =
  let outer_color =
    if Re.Pcre.pmatch ~rex:outer_re line then
      (Re.Pcre.extract ~rex:outer_re line).(1)
    else failwith ("no outer match in this | " ^ line)
  in
  let inner_matches = Re.all inner_re line in
  let num_and_colors =
    List.map inner_matches ~f:(fun group ->
        Re.Group.(
          let num = get group 1 |> Int.of_string in
          let color = get group 2 in
          (color, num)))
  in
  { color = outer_color; can_contain = StringMap.of_alist_exn num_and_colors }

let rev_rules (rules : rule list) : StringSet.t StringMap.t =
  let update_with_rule (acc : StringSet.t StringMap.t) (r : rule) =
    let container_color = r.color in
    Map.fold r.can_contain ~init:acc ~f:(fun ~key:color ~data:_ acc ->
        Map.update acc color ~f:(function
          | Some set -> Set.add set container_color
          | None -> Set.add StringSet.empty container_color))
  in

  let rec aux (acc : StringSet.t StringMap.t) (rest : rule list) =
    match rest with [] -> acc | hd :: tl -> aux (update_with_rule acc hd) tl
  in
  aux StringMap.empty rules

let print_map_sexp (m : StringSet.t StringMap.t) =
  let sexp =
    [%sexp_of: (string * string list) list]
      (Map.to_alist m |> List.map ~f:(fun (k, v_set) -> (k, Set.to_list v_set)))
  in
  print_endline (Sexp.to_string_hum sexp)

let find_all_outer_colors (lookup : StringSet.t StringMap.t) : string list =
  let my_bag = "shiny gold" in
  let starting_acc = Map.find_exn lookup my_bag |> Set.to_list in

  let rec aux (acc : StringSet.t) (rest : string list) =
    match rest with
    | [] -> acc
    | hd :: tl ->
        let new_acc = Set.add acc hd in
        let maybe_hd_outer = Map.find lookup hd in
        let new_rest =
          match maybe_hd_outer with
          | Some hd_outer ->
              Set.to_list (Set.union (StringSet.of_list tl) hd_outer)
          | None -> tl
        in
        aux new_acc new_rest
  in
  aux StringSet.empty starting_acc |> Set.to_list

let solution_pt1 (data : string list) : string =
  let rules = List.map ~f:parse_line data in
  (* let _ = List.map rules ~f:(Dbg.debug [%sexp_of: rule]) in *)
  let lookup = rev_rules rules in
  find_all_outer_colors lookup |> List.length |> Int.to_string

let solution_pt2 _ = ""
