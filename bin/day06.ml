open Core

let read_lines file =
  In_channel.with_file file ~f:(fun channel ->
    let x = In_channel.input_all channel in
    String.split_lines x)
;;

let parse_line line =
  String.split line ~on:':'
  |> List.last_exn
  |> String.split ~on:' '
  |> List.filter ~f:(fun value -> not @@ String.is_empty value)
  |> List.map ~f:(fun value -> String.strip value |> Int.of_string)
;;

let parse_line2 line =
  String.split line ~on:':'
  |> List.last_exn
  |> String.filter ~f:(fun chr -> not @@ Char.is_whitespace chr)
  |> Int.of_string
;;

let () =
  let lines = read_lines "./inputs/day06.txt" in
  let first = parse_line @@ List.nth_exn lines 0 in
  let second = parse_line @@ List.nth_exn lines 1 in
  let tuples = List.zip_exn first second in
  let result =
    List.fold tuples ~init:1 ~f:(fun acc (time, distance) ->
      acc
      * List.fold
          (List.range 1 (time + 1))
          ~init:0
          ~f:(fun acc current_time ->
            match (time - current_time) * current_time > distance with
            | true -> acc + 1
            | false -> acc))
  in
  Fmt.pr "Result of part one: %i@." result
;;

let () =
  let lines = read_lines "./inputs/day06.txt" in
  let first = parse_line2 @@ List.nth_exn lines 0 in
  let second = parse_line2 @@ List.nth_exn lines 1 in
  let result =
    List.fold
      (List.range 1 (first + 1))
      ~init:0
      ~f:(fun acc current_time ->
        match (first - current_time) * current_time > second with
        | true -> acc + 1
        | false -> acc)
  in
  Fmt.pr "Result of part two: %i@." result
;;
