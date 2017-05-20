open Printf
open Core.Std

module Pattern : sig
  type t
  val of_str : string -> t
  val to_str : t -> string
  type comparator_witness
  val comparator : (t, comparator_witness) Comparator.t
end = struct
  type t = int array

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

module Translation : sig
  type t
  val empty : t
  val overlay : t -> t -> t
  val from_words : string -> string -> t
  val consistent : t -> string -> string -> bool
  val decrypt : t -> char -> char
end = struct

  type t = {
    decrypt : int array;
    encrypt : int array;
  }

  let empty = {
    encrypt = Array.create ~len:26 (-1);
    decrypt = Array.create ~len:26 (-1)
  }

  let overlay original top =
    let original =
      { decrypt = Array.copy original.decrypt;
        encrypt = Array.copy original.encrypt } in
    Array.iteri top.decrypt ~f:(fun i value ->
      if value <> -1 then original.decrypt.(i) <- value;);
    Array.iteri top.encrypt ~f:(fun i value ->
      if value <> -1 then original.encrypt.(i) <- value;);
    original

  let num_of_char c = Char.to_int c - 97
  let char_of_num i = Char.of_int_exn (i + 97)

  let from_words crypto candidate =
      let decrypt_array = Array.create ~len:26 (-1) in
      let encrypt_array = Array.create ~len:26 (-1) in
      for i = 0 to String.length crypto - 1 do
        let crypt_char_num = num_of_char (String.get crypto i) in
        let cand_char_num = num_of_char (String.get candidate i) in
        decrypt_array.(crypt_char_num) <- cand_char_num;
        encrypt_array.(cand_char_num) <- crypt_char_num;
      done;
      { decrypt = decrypt_array; encrypt = encrypt_array }

  let consistent translation crypto candidate =
    let good = ref true in
    for i = 0 to String.length crypto - 1 do
      let crypt_char_num = num_of_char (String.get crypto i) in
      let cand_char_num = num_of_char (String.get candidate i) in
      if !good then (
        good :=
          match (translation.decrypt.(crypt_char_num), translation.encrypt.(cand_char_num)) with
          | (-1, -1) -> true
          | (-1, encrypted) when encrypted = crypt_char_num -> true
          | (decrypted, -1) when decrypted = cand_char_num -> true
          | (decrypted, encrypted) when decrypted = cand_char_num && encrypted = crypt_char_num -> true
          | (_, _) -> false;)
    done;
    !good

  let decrypt translation c = char_of_num translation.decrypt.(num_of_char c)
end

module Wordlist : sig
  type t
  val empty : t
  val cons : string -> t -> t
  val filter : t -> f:(string -> bool) -> t
  val of_list : string list -> t
  val to_list : t -> string list
  val partition_index : int -> t -> t array
  val length : t -> int
end = struct
  type t = { length : int; items : string list }

  let empty = { length = 0; items = [] }

  let filter { items; _ } ~f =
    let rec filter' ({length; items} as acc) = function
      | head::tail -> 
          if f head then
            filter' { length = length + 1; items = head::items } tail
          else filter' acc tail
      | [] -> acc in
    filter' empty items


  let cons word { length; items } = {length = length + 1; items = word::items }

  let of_list words = { length = List.length words; items = words }
  let to_list { items; _ } = items

  let partition_index i { items; _ } = 
    let result = Array.create 26 empty in
    List.iter ~f:(fun word ->
      let index = (Char.to_int (String.get word i)) - 97 in
      result.(index) <- cons word result.(index)) items;
    result

  let length { length; _ } = length
end

module Potential : sig
  type t
  val init : string list -> (Pattern.t, string list, Pattern.comparator_witness) Map.t -> t
  val expand_on_char : char -> t -> t list
  val trim : Translation.t -> t -> t
  val quantify : t -> int
  val remove_smart : t -> ((string * Wordlist.t) * t) option
  val solve : t -> Translation.t list
end = struct
  type t = (string * Wordlist.t) list

  let init words pattern_data =
    words
    |> List.dedup
    |> List.map ~f:(fun word -> 
        (word, Map.find_exn pattern_data (Pattern.of_str word) |> Wordlist.of_list))

  let expand_char_word c (word, candidates) =
    match String.index word c with
    | Some i -> (word, Wordlist.partition_index i candidates)
    | None -> (word, Array.create ~len:26 candidates)

  let expand_on_char c potential =
    let partitioned_data = List.map potential ~f:(expand_char_word c) in
    List.range 0 26
    |> List.map ~f:(fun letter_num ->
        List.map partitioned_data ~f:(fun (word, partitioned_candidates) ->
          word, partitioned_candidates.(letter_num)))

  let quantify potential =
    potential
    |> List.map ~f:(fun (_, candidates) -> Wordlist.length candidates)
    |> List.sort ~cmp:Int.compare
    |> (fun l -> List.take l 2)
    |> List.fold ~f:( * ) ~init:1

  let trim translation potential =
    List.map potential ~f:(fun (crypto, candidates) -> 
      (crypto, Wordlist.filter candidates ~f:(Translation.consistent translation crypto)))

  let remove_smart potential =
    let min_entry = 
      List.min_elt potential ~cmp:(fun (_, one) (_, two) -> 
        Int.compare (Wordlist.length one) (Wordlist.length two)) in
    Option.map min_entry (fun ((min_word, candidates) as result) ->
      (result, List.Assoc.remove potential min_word))

  let solve potential =
    let rec solve' potential =
      let least : ((string * Wordlist.t) * t) option = remove_smart potential in
      match least with
      | Some ((min_word, candidates), tail) ->
          Wordlist.to_list candidates
          |> List.map ~f:(fun curr_candidate ->
            let curr_translation = Translation.from_words min_word curr_candidate in
            let trimmed_tail = trim curr_translation tail in
            let solved = solve' trimmed_tail in
            List.map solved ~f:(Translation.overlay curr_translation)) |> List.concat
      | None -> [Translation.empty] in
    solve' potential
end

let crypto_words str = 
  str
  |> String.filter ~f:(fun c -> Char.is_alpha c || c = ' ')
  |> String.split_on_chars ~on:[' '; '\t']

let load_cryptogram filename =
  open_in filename 
  |> In_channel.input_lines 
  |> List.hd_exn
  |> crypto_words


let load_pattern_data filename =
  open_in filename
  |> In_channel.input_lines
  |> List.map ~f:(fun item -> (Pattern.of_str item, item)) 
  |> Map.of_alist_multi ~comparator:Pattern.comparator

let character_counts crypto = 
  List.range 97 (97 + 26) |> List.map ~f:Char.of_int_exn
  |> List.map ~f:(fun c ->
      (c, List.count crypto ~f:(fun word -> String.contains word c)))
  |> List.filter ~f:(fun (c, count) -> count > 1)
  |> List.sort ~cmp:(fun (c1, count1) (c2, count2) -> Int.compare count2 count1)

let solve_cryptogram crypto pattern_data =
  let initial_potential = Potential.init crypto pattern_data in
  printf "good\n%!";
  let common_letters = List.take (character_counts crypto) 2 in
  printf "good\n%!";
  let expanded_potentials =
    common_letters
    |> List.fold ~f:(fun potentials (letter, _) -> 
        List.map potentials ~f:(Potential.expand_on_char letter)
        |> List.concat) ~init:[initial_potential] in
  printf "good\n%!";
  let translations = 
    List.map [initial_potential] ~f:(fun p -> 
      printf "solving potential: %i\n%!" (Potential.quantify p);
      Potential.solve p) |> List.concat in
  List.map translations ~f:(fun translation ->
    List.map crypto ~f:(String.map ~f:(Translation.decrypt translation)))

let () =
  let pattern_data = load_pattern_data "words.txt" in
  printf "pattern data created\n%!";
  for i = 0 to 0 do
    printf "%i>" i;
    let words = read_line () |> crypto_words in
    let solutions = solve_cryptogram words pattern_data in
    let solution_lines = solutions |> List.map ~f:(String.concat ~sep:" ") in
    List.iter solution_lines ~f:(printf "%s\n%!")
  done
