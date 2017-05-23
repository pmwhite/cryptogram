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

module Letter_map : sig
  type 'a t = 'a option array
  val empty : unit -> 'a t
  val get : char -> 'a t -> 'a option
  val get_exn : char -> 'a t -> 'a
  val set : char -> 'a -> 'a t -> unit
  val overlay : 'a t -> 'a t -> 'a t
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
  type t = {
    decrypt : char Letter_map.t;
    encrypt : char Letter_map.t;
  }
  val empty : t
  val overlay : t -> t -> t
  val from_words : string -> string -> t
  val decrypt : t -> char -> char option
  val decrypt_exn : t -> char -> char
  val encrypt : t -> char -> char option
  val encrypt_exn : t -> char -> char
end = struct

  type t = {
    decrypt : char Letter_map.t;
    encrypt : char Letter_map.t;
  }

  let empty = {
    encrypt = Letter_map.empty ();
    decrypt = Letter_map.empty ()
  }

  let overlay original top =
    { decrypt = Letter_map.overlay original.decrypt top.decrypt
    ; encrypt = Letter_map.overlay original.encrypt top.encrypt }

  let from_words crypto candidate =
    let decrypt_map = Letter_map.empty () in
    let encrypt_map = Letter_map.empty () in
    for i = 0 to String.length crypto - 1 do
      let crypto_char = String.get crypto i in
      let cand_char = String.get candidate i in
      decrypt_map |> Letter_map.set crypto_char cand_char;
      encrypt_map |> Letter_map.set cand_char crypto_char
    done;
    { decrypt = decrypt_map; encrypt = encrypt_map }

  let decrypt { decrypt; _ } c = Letter_map.get c decrypt
  let decrypt_exn { decrypt; _ } c = Letter_map.get_exn c decrypt
  let encrypt { encrypt; _ } c = Letter_map.get c encrypt
  let encrypt_exn { encrypt; _ } c = Letter_map.get_exn c encrypt
end

module Candidate_set : sig
  type tree = 
    | Prefix of char * tree Letter_map.t 
    | Leaf of string

  type t = {
    candidates : tree;
    word : string;
    translation : Translation.t }
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
      if count < least + tolerance then printf "%s\n%!" solution_str;
      min least count)
  |> ignore
