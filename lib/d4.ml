let day = 4

open Core

type passport = {
  byr : string option;
  iyr : string option;
  eyr : string option;
  hgt : string option;
  hcl : string option;
  ecl : string option;
  pid : string option;
  cid : string option;
}
[@@deriving sexp]

let make_passport ?byr ?iyr ?eyr ?hgt ?hcl ?ecl ?pid ?cid () =
  { byr; iyr; eyr; hgt; hcl; ecl; pid; cid }

let passport_is_valid (p : passport) : bool =
  List.for_all ~f:Option.is_some [ p.byr; p.iyr ]

let into_groups (input_lines : string list) : string list list =
  let rec scan (acc : string list list) (curr : string list)
      (rest : string list) : string list list =
    match rest with
    | [] -> curr :: acc
    | hd :: tl ->
        if String.is_empty hd then scan (curr :: acc) [] tl
        else scan acc (hd :: curr) tl
  in
  scan [] [] input_lines

let populate_value (ident : string) (value : string) (pass : passport) :
    passport =
  match ident with
  | "byr" -> { pass with byr = Some value }
  | "iyr" -> { pass with iyr = Some value }
  | "eyr" -> { pass with eyr = Some value }
  | "hgt" -> { pass with hgt = Some value }
  | "hcl" -> { pass with hcl = Some value }
  | "ecl" -> { pass with ecl = Some value }
  | "pid" -> { pass with pid = Some value }
  | "cid" -> { pass with cid = Some value }
  | _ -> failwith ("Invalid field: " ^ ident)

let scan_line (pass : passport) (str : string) =
  match String.split_on_chars ~on:[ ':' ] str with
  | [ ident; value ] -> populate_value ident value pass
  | _ -> failwith ("the fuck? line is " ^ str)

let into_passport (group : string list) : passport =
  let everything = String.concat ~sep:" " group in
  let elements = String.split_on_chars ~on:[ ' ' ] everything in
  let valid_elements = List.filter ~f:(Fn.non String.is_empty) elements in
  let empty_passport = make_passport () in
  List.fold valid_elements ~init:empty_passport ~f:scan_line

let solution_pt1 (data : string list) : string =
  let passports = data |> into_groups |> List.map ~f:into_passport in
  passports |> List.count ~f:passport_is_valid |> Int.to_string

let solution_pt2 (data : string list) : string = String.concat ~sep:"\n" data
