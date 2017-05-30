open Printf
open Core.Std

(** Interface for dealing with patterns. Patterns represent how * characters are repeated and what positions they are in; the actual
 * characters themselves are inconsequential, only the repetitions are
 * important. For example, 'this' and 'that' have the same patterns because
 * they both have 4 different letters. 'hello' and 'patty' have the same
 * patterns because they have repeated letters in the same positions.*)
module Pattern : sig
  (** A pattern *)
  type t (** Gets the pattern for a specified string. *) val of_str : string -> t

  (** Gets a string which has the same pattern as the given pattern. Since many
   * strings can have the same pattern, there are many possible correct outputs
   * for this function. Generally, this function will give an output of the
   * form "abcd" or "abbcd", but no guaruntees are made here. *)
  val to_str : t -> string

  (** Type for creating comparison-based containers of patterns. *)
  type comparator_witness

  (** A comparator for creating comparison-based containers of patterns. *)
  val comparator : (t, comparator_witness) Comparator.t

end = struct

  (* Patterns are represented as int arrays. Each character is mapped to the
   * index at which it first occured. So "therefore" is mapped to
   * [|0;1;2;3;2;5;6;3;8|]. Note that the numbers 4 and 7 are skipped. *)
  type t = int array

  let of_str str =
    let len = String.length str in
    let result = Array.create len (-1) in
    (* For each character that hasn't been touched set every subsequent
     * occurence of the character to the index of the current character. *)
    for index = 0 to len-1 do
      (* A -1 in the result array signifies that a character hasn't been set yet. *)
      if result.(index) = -1 then (
        for i = index to len-1 do
          if String.get str i = String.get str index then 
            result.(i) <- index
        done);
    done;
    result

  (* Map each number to its letter equivalent. 0 gets translated to 'a', 1 gets
   * translated to 'b', etc. Thus, the stringified pattern for "therefore" is
   * "abcdcfgdi". Note that 'e' and 'h' are skipped just as 4 and 7 are
   * skipped. *)
  let to_str t = 
    t 
    |> Array.map ~f:(fun i -> Char.of_int_exn (i + 97)) 
    |> Array.to_list |> String.of_char_list

  (* A simple comparator for patterns. The implementation piggybacks off of
   * Array and Integer implementations because we don't care too much about how
   * patterns are compared. *)
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
  val decrypt : t -> char -> char option
  val decrypt_exn : t -> char -> char
  val encrypt : t -> char -> char option
  val encrypt_exn : t -> char -> char
end = struct

  type t = (char * char) list

  let empty = []

  let overlay original top = List.append top original

  let from_words crypto candidate = 
    List.zip_exn (String.to_list crypto) (String.to_list candidate)

  let decrypt translation c =
    List.find translation ~f:(fun (crypto_char, cand_char) -> crypto_char = c)
    |> Option.map ~f:snd

  let decrypt_exn translation c =
    List.find_exn translation ~f:(fun (crypto_char, cand_char) -> crypto_char = c)
    |> snd

  let encrypt translation c =
    List.find translation ~f:(fun (crypto_char, cand_char) -> cand_char = c)
    |> Option.map ~f:fst

  let encrypt_exn translation c =
    List.find_exn translation ~f:(fun (crypto_char, cand_char) -> cand_char = c)
    |> fst
end

module Candidate_set : sig
  type tree = 
    | Prefix of char * (char * tree) list
    | Leaf of string

  type t = {
    candidates : tree;
    word : string;
    translation : Translation.t }
  val trim_with : Translation.t -> t -> t
  val singleton : string -> string -> t
  val add_candidate_tree : tree -> char list -> char list -> string -> tree
  val add_candidate_map : (char * tree) list -> char -> char list -> char list -> string -> (char * tree) list
  val add_candidate : t -> string -> t
  val of_word_and_list : string -> string list -> t
  val to_seq : t -> string Sequence.t
  val crypto_word : t -> string
end = struct

  (** A prefix tree for efficienty storing and filtering candidate decryptions
   * for particular words.  Each node has the character of the word being
   * decrypted along; in addition, each node contains a mapping between actual
   * decrypted characters and the next nodes of the tree. This mapping is just
   * a simple prefix tree, while the character of the crypto-word is only there
   * because this is a cryptogram solver.
   *)
  type tree = 
    | Prefix of char * (char * tree) list
    | Leaf of string

  type t = {
    candidates : tree;
    word : string;
    translation : Translation.t }

  let trim_with next_translation ({ translation; _ } as c_set) =
    { c_set with translation = Translation.overlay translation next_translation }

  let rec map_find c = function
    | (cand_char, next_tree)::tl ->
        (match Char.compare cand_char c with
        | x when x < 0 -> map_find c tl
        | 0 -> Some next_tree
        | _ -> None)
    | [] -> None

  let rec to_seq ({ translation; candidates; word } as c_set) =
    match candidates with
    | Prefix (curr_crypto_char, map) -> (
        match Translation.decrypt translation curr_crypto_char with
        | Some c ->
            (match map_find c map with
            | Some next_tree -> to_seq { c_set with candidates = next_tree }
            | None -> Sequence.empty)
        | None ->
            map |> Sequence.of_list
            |> Sequence.filter_map ~f:(fun (c, tree) ->
                match Translation.encrypt translation c with
                | Some crypto_char when crypto_char <> curr_crypto_char -> None
                | None | Some _ -> Some (to_seq { c_set with candidates = tree }))
            |> Sequence.concat)
    | Leaf word -> Sequence.singleton word

  let singleton_tree crypto_chars cand_chars candidate =
    List.zip_exn crypto_chars cand_chars
    |> List.fold_right ~init:(Leaf candidate) ~f:(fun (crypto_char, cand_char) acc -> 
      Prefix (crypto_char, [(cand_char, acc)]))

  let singleton crypto candidate = {
    candidates = singleton_tree (String.to_list crypto) (String.to_list candidate) candidate;
    word = crypto;
    translation = Translation.empty }

  let rec add_candidate_tree tree cand_chars crypto_chars candidate =
      match (tree, cand_chars, crypto_chars) with
      | (Prefix(c, letter_map), cand_hd::cand_tl, crypto_hd::crypto_tl) ->
          Prefix(c, add_candidate_map letter_map cand_hd cand_tl crypto_tl candidate)
      | _ -> failwith (sprintf "bad: %s %s %s" candidate (String.of_char_list cand_chars) (String.of_char_list crypto_chars))

  and add_candidate_map letter_map cand_hd cand_tl crypto_tl candidate =
    match letter_map with
    | ((cand_char, next_tree)::tl) as curr_map ->
        (match Char.compare cand_hd cand_char with
        | x when x > 0 -> (cand_char, next_tree)::(add_candidate_map tl cand_hd cand_tl crypto_tl candidate)
        | 0 -> (cand_char, add_candidate_tree next_tree cand_tl crypto_tl candidate)::tl
        | _ -> (cand_hd, singleton_tree crypto_tl cand_tl candidate)::curr_map)
    | [] -> [(cand_hd, singleton_tree crypto_tl cand_tl candidate)]

  let add_candidate ({ word; candidates; translation } as c_set) candidate =
    let cand_chars = String.to_list candidate in
    let crypto_chars = String.to_list word in
    { c_set with candidates = add_candidate_tree candidates cand_chars crypto_chars candidate }

  let of_word_and_list crypto candidates =
    match candidates with
    | hd::tl ->
        List.fold tl ~f:add_candidate ~init:(singleton crypto hd)
    | [] -> failwith "cannot make a Candidate_set without any candidate words."

  let crypto_word { word; _ } = word
end

module Potential : sig
  type t
  val init : string list -> (Pattern.t, string list, Pattern.comparator_witness) Map.t -> t
  val trim_with : Translation.t -> t -> t
  val solve : t -> Translation.t Sequence.t
  val order_by_effect : t -> t
end = struct
  type t = Candidate_set.t list

  let init words pattern_data =
    words
    |> List.dedup
    |> List.map ~f:(fun word -> 
        Map.find_exn pattern_data (Pattern.of_str word) |> Candidate_set.of_word_and_list word)

  let trim_with translation potential = List.map potential ~f:(Candidate_set.trim_with translation)

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
    let words = List.map potential ~f:Candidate_set.crypto_word in
    let orderered_words = order' words "" in
    List.map orderered_words ~f:(fun word -> 
      List.find_exn potential ~f:(fun c_set ->
          Candidate_set.crypto_word c_set = word))

  let solve potential =
    let rec solve' = function
      | c_set::tail ->
          let min_word = Candidate_set.crypto_word c_set in
          Candidate_set.to_seq c_set
          |> Sequence.map ~f:(fun curr_candidate ->
            let curr_translation = Translation.from_words min_word curr_candidate in
            let trimmed_tail = trim_with curr_translation tail in
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
    List.map crypto ~f:(String.map ~f:(Translation.decrypt_exn translation)))

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
      if count < least + tolerance then printf "%s: %d\n%!" solution_str count;
      min least count)
  |> ignore
