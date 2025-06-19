open Stdio

let data_for_day (day : int) : string list =
  In_channel.read_lines ("data/d" ^ Int.to_string day)
