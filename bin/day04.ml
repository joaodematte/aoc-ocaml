open Core

let read_lines file =
  Stdio.In_channel.with_file file ~f:(fun channel ->
    let x = In_channel.input_all channel in
    String.split_lines x)
;;

let parse_line line =
  let separator_pos = String.index_exn line ':' + 1 in
  let parse_numbers cards =
    String.split cards ~on:' '
    |> List.filter ~f:(fun str -> not @@ String.equal str "")
    |> List.map ~f:(fun number -> String.strip number |> Int.of_string)
  in
  let content =
    String.sub line ~pos:separator_pos ~len:(String.length line - separator_pos)
    |> String.split ~on:'|'
    |> List.map ~f:parse_numbers
  in
  List.hd_exn content, List.last_exn content
;;

let intersection a b = List.filter a ~f:(fun e -> List.mem b e ~equal:( = ))

let resolve_part_one lines =
  List.fold lines ~init:0 ~f:(fun acc line ->
    let winning_cards, hand = parse_line line in
    let intersection = intersection winning_cards hand in
    let raw_points = List.length intersection in
    match raw_points with
    | 0 -> acc
    | 1 -> acc + 1
    | x -> acc + Int.pow 2 (x - 1))
;;

(* I didn't quite understand why this works,
   my brain was working on auto suggestions
   and I eventually ended up with a working code
*)
let resolve_part_two lines =
  let counter = Array.init (List.length lines) ~f:(fun _ -> 1) in
  List.iteri lines ~f:(fun index line ->
    let winning_cards, hand = parse_line line in
    let match_count = List.length @@ intersection winning_cards hand in
    for match_index = 0 to match_count - 1 do
      counter.(index + 1 + match_index)
      <- counter.(index + 1 + match_index) + counter.(index)
    done);
  List.fold_left (Array.to_list counter) ~init:0 ~f:( + )
;;

let () =
  let lines = read_lines "./inputs/day04.txt" in
  Fmt.pr "Result of part one: %i@." @@ resolve_part_one lines;
  Fmt.pr "Result of part two: %i@." @@ resolve_part_two lines
;;
