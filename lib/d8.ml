let day = 8

open Core

type op = NOP | ACC | JMP [@@deriving sexp]
type ins = { op : op; value : int } [@@deriving sexp]
type exit = { acc : int; end_loc : int } [@@deriving sexp]

let line_pattern = Re.Pcre.regexp {|^(acc|jmp|nop) ([+-]\d+)$|}

let parse_line (line : string) : ins =
  let m = Re.exec line_pattern line in
  let op =
    match Re.Group.get m 1 with
    | "acc" -> ACC
    | "jmp" -> JMP
    | "nop" -> NOP
    | unsupported -> failwith ("unsupported instruction " ^ unsupported)
  in
  let value = Re.Group.get m 2 |> Int.of_string in
  { op; value }

let run_program (program : ins array) : exit =
  let program_len = Array.length program in
  let visited = Int.Hash_set.create () in
  let rec run (curr_pos : int) (acc : int) =
    Hash_set.add visited curr_pos;
    if curr_pos >= program_len then { acc; end_loc = curr_pos }
    else
      let ins = program.(curr_pos) in
      match ins.op with
      | ACC -> run (curr_pos + 1) (acc + ins.value)
      | NOP -> run (curr_pos + 1) acc
      | JMP ->
          let next_pos = curr_pos + ins.value in
          if Hash_set.mem visited next_pos then { acc; end_loc = curr_pos }
          else run next_pos acc
  in
  run 0 0

let solution_pt1 (data : string list) : string =
  let program = List.map data ~f:parse_line |> Array.of_list in
  let exit = run_program program in
  exit.acc |> Int.to_string

let convert (ins : ins) : ins =
  match ins.op with
  | NOP -> { ins with op = JMP }
  | JMP -> { ins with op = NOP }
  | _ -> failwith "Can't convert!"

let find_nop_or_jmp_idx (idx : int) (acc : int list) (ins : ins) : int list =
  match ins.op with NOP | JMP -> idx :: acc | _ -> acc

let find_candidates (program : ins array) : int list =
  let candidates = Array.foldi program ~init:[] ~f:find_nop_or_jmp_idx in
  candidates

let run_program_with_repair (program : ins array) (loc : int) : exit =
  let program_copy = Array.copy program in
  let old_ins_at_loc = Array.get program loc in
  let new_ins = convert old_ins_at_loc in
  Array.set program_copy loc new_ins;
  run_program program_copy

let happy_exit (program : ins array) (ex : exit) : bool =
  ex.end_loc = Array.length program

let solution_pt2 (data : string list) : string =
  let program = List.map data ~f:parse_line |> Array.of_list in
  let candidates = find_candidates program in
  let trial_runs =
    List.map candidates ~f:(fun candidate ->
        run_program_with_repair program candidate)
  in
  let successful_run =
    List.find_exn trial_runs ~f:(fun run -> happy_exit program run)
  in
  successful_run.acc |> Int.to_string
