open Printf
open Core.Std

let stream_filter p stream =
  let rec next i =
    try
      let value = Stream.next stream in
      if p value then Some value else next i
    with Stream.Failure -> None in
  Stream.from next

let stream_take n stream =
  let num_left = ref n in
  let rec next i =
    try
      let value = Stream.next stream in
      if !num_left > 0 then (
        num_left := !num_left - 1;
        Some value )
      else None
    with Stream.Failure -> None in
  Stream.from next

let line_stream channel =
  Stream.from (fun _ -> try Some (input_line channel) with End_of_file -> None)

let () =
  if Array.length Sys.argv = 1 then
    printf "no args; please give an arg; the arg should be the wordlist filename"
  else
    let filename = Sys.argv.(1) in
    let file_channel = open_in filename in
    printf "Word file: %s\n" filename;
    let first_ten = stream_take 2000 (line_stream file_channel) in
    let count = ref 0 in
    Stream.iter (fun line -> 
      count := !count + 1;
      if !count mod 100000 = 0 then
        printf "%s\n" line;) (line_stream file_channel)
