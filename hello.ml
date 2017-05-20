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
  val filter : t -> f:(string -> bool) -> t
  val of_list : string list -> t
  val to_seq : t -> string Sequence.t
end = struct
  type t = string Sequence.t

  let empty = Sequence.empty

  let filter = Sequence.filter

  let of_list = Sequence.of_list
  let to_seq = ident
end

module Potential : sig
  type t
  val init : string list -> (Pattern.t, string list, Pattern.comparator_witness) Map.t -> t
  val trim : Translation.t -> t -> t
  val solve : t -> Translation.t Sequence.t
  val order_by_effect : t -> t
end = struct
  type t = (string * Wordlist.t) list

  let init words pattern_data =
    words
    |> List.dedup
    |> List.map ~f:(fun word -> 
        (word, Map.find_exn pattern_data (Pattern.of_str word) |> Wordlist.of_list))

  let trim translation potential =
    List.map potential ~f:(fun (crypto, candidates) -> 
      (crypto, Wordlist.filter candidates ~f:(Translation.consistent translation crypto)))

  let order_by_effect potential =
    let rec order' words used =
      let set = Set.of_list words ~comparator:String.comparator in
      let effects : (string * int) list = Set.map set ~f:(fun word -> 
        let rest = Set.remove set word in
        let str = 
          Set.to_list rest |> String.concat 
          |> String.filter ~f:(String.contains (word ^ used)) in
        let effect = 
          String.fold word ~init:0 ~f:(fun total c -> 
            total + (String.count str ~f:((=) c))) in
        (word, effect)) ~comparator:Comparator.Poly.comparator |> Set.to_list in
      match List.max_elt effects ~cmp:(fun (w1, e1) (w2,e2) -> Int.compare e1 e2) with
      | Some (max_word, max_effect) ->
          let rest = List.Assoc.remove effects max_word |> List.map ~f:fst in
          max_word::(order' rest (max_word ^ used))
      | None -> [] in
    let words = List.map potential ~f:fst in
    let orderered_words = order' words "" in
    List.map orderered_words ~f:(fun word -> (word, List.Assoc.find_exn potential word))

  let solve potential =
    let rec solve' = function
      | (min_word, candidates)::tail ->
          Wordlist.to_seq candidates
          |> Sequence.map ~f:(fun curr_candidate ->
            let curr_translation = Translation.from_words min_word curr_candidate in
            let trimmed_tail = trim curr_translation tail in
            let solved = solve' trimmed_tail in
            Sequence.map solved ~f:(Translation.overlay curr_translation)) |> Sequence.concat
      | [] -> Sequence.singleton Translation.empty in
    solve' (order_by_effect potential)

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

let load_counts filename =
  open_in filename
  |> In_channel.input_lines
  |> List.mapi ~f:(fun i item -> (item, i))
  |> Map.of_alist_exn ~comparator:String.comparator

let solve_cryptogram crypto pattern_data =
  let initial_potential = Potential.init crypto pattern_data in
  let translations = Potential.solve initial_potential in
  Sequence.map translations ~f:(fun translation ->
    List.map crypto ~f:(String.map ~f:(Translation.decrypt translation)))

let () =
  let pattern_data = load_pattern_data "words.txt" in
  let counts = load_counts "words.txt" in
  printf "dictionary loaded\n%!";
  let words = read_line () |> crypto_words in
  let tolerance = 100 in
  solve_cryptogram words pattern_data
  |> Sequence.fold ~init:(Int.max_value - tolerance) ~f:(fun least solution ->
      let solution_str = String.concat solution ~sep: " " in
      let count = 
        List.map solution ~f:(Map.find_exn counts)
        |> List.fold ~init:0 ~f:(+) in
      if count < least + tolerance then printf "%s\n%!" solution_str;
      min least count)
  |> ignore
