open Printf
open Core.Std

type pattern_char =
  | Ground of char
  | Wild of int

type pattern = pattern_char array

let str_to_pattern str legend =
  let len = String.length str in
  let result = Array.create len (Wild (-1)) in
  let str_array = str |> String.to_array in
  for index = 0 to len-1 do
    match result.(index) with
    | Wild -1 -> (
        let curr_char = str_array.(index) in
        match Map.find legend curr_char with
        | Some solution_char -> ()
        | None -> 
            for i = index to len-1 do
              if str_array.(i) = curr_char then result.(i) <- Wild index
            done)
    | Wild _ -> ()
    | Ground _ -> ()
  done;
  result |> Array.to_list

let pattern_to_str pattern =
  pattern |> List.map ~f:(function
    | Ground c -> Char.uppercase c
    | Wild i -> Char.of_int_exn (97 + i))
  |> String.of_char_list

let lines filename = open_in filename |> In_channel.input_lines

let read_crypto str =
  str |> String.to_list 
  |> List.filter ~f:(fun c -> Char.is_alpha c || c = ' ')
  |> List.map ~f:(fun c -> if Char.is_whitespace c then ' 'else c)
  |> String.of_char_list |> String.split ~on:' '


let () =
  if Array.length Sys.argv < 3 then
    printf "Please pass in at least two arguments"
  else
    printf "loading dictionary...\n%!";
    let wordlist = lines Sys.argv.(1) in
    printf "dictionary loaded.\n%!";
    match Sys.argv.(2) |> lines |> List.hd with
    | None -> failwith "Cryptogram file is empty"
    | Some str ->
        let legend = Map.empty Char.comparator in
        let patterns = wordlist |> Sequence.of_list |> Sequence.map ~f:(fun word -> str_to_pattern word legend) in
        Sequence.iteri patterns ~f:(fun i pattern -> 
          if i mod 100000 = 0 then printf "%s\n%!" (pattern_to_str pattern););
