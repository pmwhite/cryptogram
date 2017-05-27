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

(** Operations on letter maps. One could use actual maps keyed on characters,
 * but these should be faster because we know that there are a maximum of 26
 * possible letters and we can get O(1) access for a specific letter. This
 * module is here mostly for performance reasons, as well as a little bit of
 * convenience. *)
module Letter_map : sig
  (** A letter map which stores items of type 'a *)
  type 'a t

  (** Creates and empty letter map *)
  val empty : unit -> 'a t

  (** Gets the value associated with the given character. The character may not
   * have an associated value, so an option is returned *)
  val get : char -> 'a t -> 'a option

  (** Gets the value associated with the given character, assuming the
   * character is in the map. *)
  val get_exn : char -> 'a t -> 'a

  (** Mutation of a value in the map; use with care. This function will modify all copies of the letter map. Best used on letter maps created using `empty` because you know there are no copies. *)
  val set : char -> 'a -> 'a t -> unit

  (** Union of the two letter maps. *)
  val overlay : 'a t -> 'a t -> 'a t

  (** Get a sequence of the key-value pairs of the letter map *)
  val to_seq : 'a t -> (char * 'a) Sequence.t
end = struct
  type 'a t = 'a option array

  let empty () = Array.create ~len:26 None

  let num_of_char c = Char.to_int c - 97
  let char_of_num i = Char.of_int_exn (i + 97)

  let get c letter_map =
    letter_map.(num_of_char c)

  let get_exn c letter_map = Option.value_exn (get c letter_map)

  let set c x letter_map =
    letter_map.(num_of_char c) <- Some x

  let overlay bottom top =
    let result = Array.copy bottom in
    Array.iteri top ~f:(fun i value ->
      match value with
      | (Some _) -> result.(i) <- value
      | None -> ());
    result


  let to_seq letter_map =
    letter_map |> Array.to_sequence |> Sequence.filter_mapi ~f:(fun i x -> 
      Option.map x ~f:(fun item -> (char_of_num i, item)))
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
  type t
  val trim_with : Translation.t -> t -> t
  val singleton : string -> string -> t
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
    | Prefix of char * tree Letter_map.t 
    | Leaf of string

  type t = {
    candidates : tree;
    word : string;
    translation : Translation.t }

  let trim_with next_translation ({ translation; _ } as c_set) =
    { c_set with translation = Translation.overlay translation next_translation }

  let rec to_seq ({ translation; candidates; word } as c_set) =
    match candidates with
    | Prefix (curr_crypto_char, map) -> (
        match Translation.decrypt translation curr_crypto_char with
        | Some c -> 
            (match Letter_map.get c map with
            | Some next_tree -> to_seq { c_set with candidates = next_tree }
            | None -> Sequence.empty)
        | None ->
            map |> Letter_map.to_seq
            |> Sequence.filter_map ~f:(fun (c, tree) ->
                match Translation.encrypt translation c with
                | Some crypto_char when crypto_char <> curr_crypto_char -> None
                | None | Some _ -> Some (to_seq { c_set with candidates = tree }))
            |> Sequence.concat)
    | Leaf word -> Sequence.singleton word

  let singleton_tree crypto_chars cand_chars candidate =
    List.zip_exn crypto_chars cand_chars
    |> List.fold_right ~init:(Leaf candidate) ~f:(fun (crypto_char, cand_char) acc -> 
      let letter_map = Letter_map.empty () in
      Letter_map.set cand_char acc letter_map;
      Prefix (crypto_char, letter_map))

  let singleton crypto candidate = {
    candidates = singleton_tree (String.to_list crypto) (String.to_list candidate) candidate;
    word = crypto;
    translation = Translation.empty }

  let add_candidate { word; candidates; translation } candidate =
    let cand_chars = String.to_list candidate in
    let crypto_chars = String.to_list word in
    let rec add_candidate' tree crypto_chars cand_chars =
      match (tree, cand_chars, crypto_chars) with
      | (Prefix(_, letter_map), cand_hd::cand_tl, crypto_hd::crypto_tl) ->
          (match Letter_map.get cand_hd letter_map with
          | Some next_tree -> add_candidate' next_tree crypto_tl cand_tl
          | None -> letter_map |> Letter_map.set cand_hd (singleton_tree crypto_tl cand_tl candidate))
      | _ -> failwith (sprintf "bad candidate: word %s; cand %s; word_p %s; cand_p %s" word candidate (String.of_char_list crypto_chars) (String.of_char_list cand_chars)) in
    add_candidate' candidates crypto_chars cand_chars

  let of_word_and_list crypto candidates =
    match candidates with
    | hd::tl ->
        let result : t = singleton crypto hd in
        List.iter tl ~f:(add_candidate result);
        result
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
