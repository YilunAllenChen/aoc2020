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
  let lookup = rev_rules rules in
  find_all_outer_colors lookup |> List.length |> Int.to_string

let rules_to_lookup (rules : rule list) : int StringMap.t StringMap.t =
  List.fold rules ~init:StringMap.empty
    ~f:(fun (acc : int StringMap.t StringMap.t) (r : rule) ->
      Map.update acc r.color ~f:(function
        | Some _ -> failwith "the fuck"
        | None -> r.can_contain))

let rec capacity_of (curr : string) (lookup : int StringMap.t StringMap.t) : int
    =
  let maybe_inner = Map.find_exn lookup curr in
  if Map.is_empty maybe_inner then 1
  else
    let kv_lst = Map.to_alist maybe_inner in
    List.fold kv_lst ~init:1 ~f:(fun (acc : int) ((key, v) : string * int) ->
        let sub_capacity = v * capacity_of key lookup in
        (* print_endline *)
        (*   (curr ^ " has capacity contributed from : " ^ key ^ " : " *)
        (*  ^ Int.to_string sub_capacity); *)
        acc + sub_capacity)

let solution_pt2 (data : string list) : string =
  let rules = List.map ~f:parse_line data in
  let lookup = rules_to_lookup rules in
  (* let my_bag = Map.find_exn lookup "shiny gold" in *)
  (* let _ = Map.to_alist my_bag |> Dbg.debug [%sexp_of: (string * int) list] in *)
  capacity_of "shiny gold" lookup - 1 |> Int.to_string
