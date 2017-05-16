open Printf
open Core.Std

module Pattern = struct
  let of_str str =
    let len = String.length str in
    let result = Array.create len (-1) in
    for index = 0 to len-1 do
      if result.(index) = -1 then (
        for i = index to len-1 do
          if String.get str i = String.get str index then 
            result.(i) <- index
        done);
    done;
    result

  let to_str t =
    t |> Array.map ~f:Char.of_int_exn |> Array.to_list |> String.of_char_list

  include Comparator.Make(struct
    type t = int array
    let sexp_of_t = Array.sexp_of_t Int.sexp_of_t
    let t_of_sexp = Array.t_of_sexp Int.t_of_sexp
    let compare = Array.compare Int.compare
  end)
end

type potential = (string * string list) list

let lines filename = open_in filename |> In_channel.input_lines

let crypto_words str =
  str
  |> String.filter ~f:(fun c -> Char.is_alpha c || c = ' ')
  |> String.split_on_chars ~on:[' '; '\t']

let group_by list ~f ~comparator =
  list 
  |> List.map ~f:(fun item -> (f item, item)) 
  |> Map.of_alist_multi ~comparator:comparator


let pattern_data = group_by ~f:Pattern.of_str ~comparator:Pattern.comparator

let initial_potential words pattern_data =
  words
  |> List.dedup
  |> List.map ~f:(fun word -> 
      (word, Map.find_exn pattern_data (Pattern.of_str word)))

let letters = [
  'a';'b';'c';'d';'e';'f';
  'g';'h';'i';'j';'k';'l';
  'm';'n';'o';'p';'q';'r';
  's';'t';'u';'v';'w';'x';
  'y';'z' ]

let expand_char_word c (word, candidates) =
  let partitioned_candidates = 
    match String.index word c with
    | Some i ->
        candidates 
        |> group_by ~f:(fun candidate -> String.get candidate i) ~comparator:Char.comparator
    | None ->
        letters
        |> List.map ~f:(fun letter -> (letter, candidates))
        |> Map.of_alist_exn ~comparator:Char.comparator in
  (word, partitioned_candidates)


let expand_char_potential c potential =
  let partitioned_data = List.map potential ~f:(expand_char_word c) in
  letters
  |> List.map ~f:(fun letter ->
      List.map partitioned_data ~f:(fun (word, partitioned_candidates) ->
        (word, Map.find_exn partitioned_candidates letter)))

let character_counts (words : string list) = 
  letters
  |> List.map ~f:(fun c ->
      let count = 
        words 
        |> List.map ~f:(String.count ~f:((=) c)) 
        |> List.fold ~init:0 ~f:(+) in
      (c, count))
  |> List.filter ~f:(fun (c, count) -> count > 0)
  |> List.sort ~cmp:(fun (c1, count1) (c2, count2) -> Int.compare count2 count1)

let () =
  if Array.length Sys.argv < 3 then
    printf "Please pass in at least two arguments"
  else
    let wordlist = lines Sys.argv.(1) in
    printf "dictionary loaded.\n%!";
    let p_data = pattern_data wordlist in
    printf "pattern data created: %d\n%!" (Map.length p_data);
    match Sys.argv.(2) |> lines |> List.hd with
    | None -> failwith "Cryptogram file is empty"
    | Some str ->
        wordlist 
        |> Sequence.of_list 
        |> Sequence.iteri ~f:(fun i word -> 
          if i mod 100000 = 0 then printf "%s\n%!" word;);
