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
  List.for_all ~f:Option.is_some
    [ p.byr; p.iyr; p.eyr; p.hgt; p.hcl; p.ecl; p.pid ]

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

let opt_num_between (num_str : string option) (lb : int) (ub : int) : bool =
  Option.bind num_str ~f:Int.of_string_opt
  |> Option.filter ~f:(Int.between ~low:lb ~high:ub)
  |> Option.is_some

let is_valid_hgt (hgt_str : string option) : bool =
  let num_from_char_list (lst : char list) : int option =
    String.of_list lst |> Int.of_string_opt
  in

  let rec parse_height (numeric : char list) (rest : char list) =
    match rest with
    | [] -> false
    | [ 'i'; 'n' ] ->
        numeric |> List.rev |> num_from_char_list
        |> Option.filter ~f:(Int.between ~low:59 ~high:76)
        |> Option.is_some
    | [ 'c'; 'm' ] ->
        numeric |> List.rev |> num_from_char_list
        |> Option.filter ~f:(Int.between ~low:150 ~high:193)
        |> Option.is_some
    | hd :: tl ->
        if Char.is_digit hd then parse_height (hd :: numeric) tl else false
  in

  match hgt_str with
  | Some hgt -> parse_height [] (String.to_list hgt)
  | None -> false

let is_valid_hcl (hcl : string option) : bool =
  let is_valid_char (ch : char) : bool =
    Char.is_digit ch
    || List.exists [ 'a'; 'b'; 'c'; 'd'; 'e'; 'f' ] ~f:(Char.equal ch)
  in
  let is_valid_hcl_str (hcl_str : string) : bool =
    match String.to_list hcl_str with
    | '#' :: rest -> List.length rest = 6 && List.for_all rest ~f:is_valid_char
    | _ -> false
  in
  match hcl with Some hcl_str -> is_valid_hcl_str hcl_str | None -> false

let is_valid_ecl : string option -> bool = function
  | None -> false
  | Some ecl -> (
      match ecl with
      | "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth" -> true
      | _ -> false)

let is_valid_pid (pid_str : string option) : bool =
  pid_str
  |> Option.filter ~f:(fun str -> String.length str = 9)
  |> Option.bind ~f:Int.of_string_opt
  |> Option.filter ~f:(fun pid -> pid < 1_000_000_000)
  |> Option.is_some

let passport_is_valid_pt2 (p : passport) : bool =
  let byr_ok = opt_num_between p.byr 1920 2002 in
  let iyr_ok = opt_num_between p.iyr 2010 2020 in
  let eyr_ok = opt_num_between p.eyr 2020 2030 in
  let hgt_ok = is_valid_hgt p.hgt in
  let hcl_ok = is_valid_hcl p.hcl in
  let ecl_ok = is_valid_ecl p.ecl in
  let pid_ok = is_valid_pid p.pid in
  List.for_all ~f:Fn.id
    [ byr_ok; iyr_ok; eyr_ok; hgt_ok; hcl_ok; ecl_ok; pid_ok ]

let solution_pt2 (data : string list) : string =
  let passports = data |> into_groups |> List.map ~f:into_passport in
  passports |> List.count ~f:passport_is_valid_pt2 |> Int.to_string
