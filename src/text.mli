(*
 * text.mli
 * --------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

(** UTF-8 encoded strings *)

(** This modules is intended for ``text'' manipulation. By text we
    mean sequence of unicode characters.

    For compatibility and simplicity reasons, text is represented by
    UTF-8 encoded strings, and there is no special types for unicode
    characters, whose are just represented by 1-length text.

    All functions of this module expect to by applied on valid UTF-8
    encoded strings, and may raise [Invalid] if this is not the case.
*)

type t = string
    (** Type of text *)

exception Invalid of string * string
  (** [Invalid(error, text)] Exception raised when an invalid UTF-8
      encoded string is encountered. [text] is the faulty text and
      [error] is a description of the first error in [text]. *)

val check : string -> string option
  (** [check str] checks that [str] is a valid UTF-8 encoded
      string. Returns [None] if it is the case, or [Some error]
      otherwise. *)

val validate : string -> unit
  (** Same as check but raises an exception in case the argument is
      not a valid text. *)

(** {6 Encoding/decoding} *)

val encode : ?encoding : Encoding.t -> t -> string
  (** [encode ?encoding txt] encode the given text with [encoding],
      which defaults to {!Encoding.system} plus transliteration. *)

val decode : ?encoding : Encoding.t -> string -> t
  (** [decode ?encoding str] decode the given string encoded in
      [encoding], which defaults to {!Encoding.system} *)

val to_ascii : t -> t
  (** [to_ascii txt] returns an approximative ascii version of
      [txt]. This is the same as [encode ~encoding:"ASCII//TRANSLIT" txt] *)

(** {6 Informations} *)

val length : t -> int
  (** Return the number of unicode character contained in the given
      text *)

(** {6 Construction/access} *)

val code : t -> int
  (** [code text] returns the unicode code-point of first character of
      [text].

      For example:

      - [code "A" = 65]
      - [code "é" = 0xe9]
  *)

val char : int -> t
  (** [char code] returns the character corresponding to the given
      unicode code-point.

      For example:

      - [char 65 = "A"]
      - [char 0xe9 = "é"]

      @raise Invalid_argument if [code] is not a valid unicode
      code-point. Valid code-point are all integers in the range
      [0..0x10ffff]. *)

val get : t -> int -> t
  (** [get text n] returns the [n]-th character of [text]. [n] is a
      number of unicode character, not bytes. A negative value is
      interpreted as a position from the end of the text.

      For example:

      - [get "abc" 0 = "a"]
      - [get "abc" 2 = "c"]
      - [get "aéb" 1 = "é"]
      - [get "aéb" 2 = "b"]
      - [nth "abc" (-1) = "c"]
  *)

val sub : t -> int -> int -> t
  (** [sub text pos len] Returns the sub-text of [text] starting at
      position [pos] and of length [len]. [pos] and/or [len] may be
      negative.

      For example:

      - [sub "ocaml" 1 2 = "ca"]
      - [sub "ocaml" 3 (-2) = "ca"]
      - [sub "ocaml" (-2) 1 = "m"]
  *)

val slice : t -> int -> int -> t
  (** [slice text a b] returns the text contained in [txt] between [a]
      and [b] (exlusive). [a] and/or [b] may be negative.

      For example:

      - [slice "abc" 0 1 = "a"]
      - [slice "abcdef" 1 (-1) = "bcde"]
  *)

val splice : t -> int -> int -> t -> t
  (** [splice text a b repl] replace the text between [a] and [b]
      (exclusive) by [repl].

      For example:

      - [splice "abcd" 1 2 "plop" = "aplopcd"]
      - [splice "abcd" 1 2 "" = "acd"]
  *)

val repeat : int -> t -> t
  (** [repeat n text] returns [text] concatened [n]-times with
      itself. *)

val init : int -> (int -> t) -> t
  (** [init n f] returns [f 0 ^ f 1 ^ ... ^ f (n - 1)] *)

val rev_init : int -> (int -> t) -> t
  (** [rev_init n f] returns [f (n - 1) ^ f 1 ^ ... ^ f 0] *)

(** {6 Locale specific functions} *)

val upper : t -> t
  (** [upper t] returns the upper-cased version of [t]. *)

val lower : t -> t
  (** [lower t] returns the upper-cased version of [t]. *)

val capitalize : t -> t
  (** [capitalize t] returns [t] with its first letter upper-cased *)

val uncapitalize : t -> t
  (** [capitalize t] returns [t] with its first letter lower-cased *)

val compare : t -> t -> int
  (** Compares two texts according to the current locale *)

val icompare : t -> t -> int
  (** Compares two texts, case-insensitive *)

val transform : t -> t
  (** [transform str] transforms [str] in a way such that comparing
      two string [str1] and [str2] transformed with
      [Pervasives.compare] give the same result as comparing them with
      {!compare}. *)

(** {6 Transformations} *)

val rev : t -> t
  (** [rev t] returns the sequence of characters of [t] in reverse
      order.

      For example:

      - [rev "ocaml" = "lmaco"]
      - [rev "héhé" = "éhéh"]
  *)

val concat : t -> t list -> t
  (** [concat sep l] returns the concatenation of all texts contained
      in [l], separated by [sep].

      For example:

      - [concat "/" ["a"; "b"; "c"] = "a/b/c"]
  *)

val rev_concat : t -> t list -> t
  (** [rev_concat sep l] returns the concatenation of all texts
      contained in [l], separated by [sep].

      For example:

      - [concat "/" ["a"; "b"; "c"] = "c/b/a"]
 *)

val explode : t -> t list
  (** [explode txt] returns the list of all characters of [txt].

      For example:

      - [explode "" = []]
      - [explode "abé" = ["a"; "b"; "é"]]
  *)

val rev_explode : t -> t list
  (** [rev_explode txt] returns the list of all characters of [txt],
      in reverse order.

      For example:

      - [rev_explode "ocaml" = ["l"; "m"; "a"; "c"; "o"]]
  *)

val implode : t list -> t
  (** [implode l] returns the concatenation of all texts contained in
      [l]. This is the same as [concat "" l], but a bit more
      efficient.

      For example:

      - [implode ["o"; "c"; "a"; "m"; "l"] = "ocaml"]
      - [implode ["abc"; "def"] = "abcdef"]
  *)

val rev_implode : t list -> t
  (** [rev_implode l] returns the concatenation of all texts contained
      in [l], in reverse order.

      For example:

      - [implode ["o"; "c"; "a"; "m"; "l"] = "lmaco"]
      - [implode ["abc"; "def"] = "defabc"]
  *)

(** {6 Tests} *)

(** The following functions tests whether all characters of the given
    text verify a property: *)

val is_ascii : t -> bool
val is_alnum : t -> bool
val is_alpha : t -> bool
val is_blank : t -> bool
val is_cntrl : t -> bool
val is_digit : t -> bool
val is_graph : t -> bool
val is_lower : t -> bool
val is_print : t -> bool
val is_punct : t -> bool
val is_space : t -> bool
val is_upper : t -> bool
val is_xdigit : t -> bool

(** {6 Text traversals} *)

(** For all the following functions we give a equivalent
    implementation, and examples. They have the same semantic as the
    equivalent implementation but are more efficient. *)

val map : (t -> t) -> t -> t
  (** [map f text] ~ [implode (List.map f (explode text))]

      [map (function "a" -> "x" | t -> t) "abc" = "xbc"] *)

val rev_map : (t -> t) -> t -> t
  (** [rev_map f text] ~ [implode (List.rev_map f (explode text))]

      [rev_map (function "a" -> "x" | t -> t) "abc" = "cbx"] *)

val fold : (t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f x text] ~ [List.fold_left f x (explode text)]

      [fold (fun acc t -> acc + code t) 0 "ABC" = 198] *)

val rev_fold : (t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f text x] ~ [List.fold_left f x (rev_explode text)]

      [rev_fold (fun t acc -> acc + code t) "ABC" 0 = 198] *)

val filter : (t -> bool) -> t -> t
  (** [filter text] ~ [implode (List.filter f (explode text))]

      [filter is_alpha "1a2E" = "aE"] *)

val rev_filter : (t -> bool) -> t -> t
  (** [rev_filter text] ~ [implode (List.filter f (rev_explode text))]

      [rev_filter is_alpha "1a2E" = "Ea"] *)

val iter : (t -> unit) -> t -> unit
  (** [iter f text] ~ [List.iter f (explode text)] *)

val rev_iter : (t -> unit) -> t -> unit
  (** [iter f text] ~ [List.iter f (rev_explode text)] *)

(** {6 Scanning} *)

val for_all : (t -> bool) -> t -> bool
  (** [for_all f text] returns whether all characters of [text] verify
      the predicate [f] *)

val exists : (t -> bool) -> t -> bool
  (** [exists f text] returns whether at least one character of [text]
      verify [f] *)

val count : (t -> bool) -> t -> int
  (** [count f text] returhs the number of characters of [text]
      verifying [f] *)

(** {6 Splitting} *)

val words : t -> t list
  (** Returns all words of the given text. Words are sequence of
      non-space and non-punct characters. *)

val lines : t -> t list
  (** Returns all lines of the given text, without end of line
      characters. Both ["\r\n"] and ["\n"] are recognized as end of
      line delimiters. *)

val split : ?max : int -> ?sep : t -> t -> t list
  (** [split ?max ?sep text] split [text] according to [sep]. If [max]
      is specified, returns at most [max] splits. [sep] defaults to [" "].

      For example:

      - [split ~sep:"/" "a/b/c" = ["a"; "b"; "c"]]
      - [split ~sep:".." "a..b..c" = ["a"; "b"; "c"]]
      - [split ~max:1 "a b c" = ["a b c"]]
      - [split ~max:2 "a b c" = ["a"; "b c"]]
  *)

val rev_split : ?max : int -> ?sep : t -> t -> t list
  (** [rev_split ?max text sep] split [text] according to [sep] in reverse
      order.

      For example:

      - [split ~sep:"/" "a/b/c" = ["c"; "b"; "a"]]
      - [split ~max:1 "a b c" = ["a b c"]]
      - [split ~max:2 "a b c" = ["a b"; "c"]]
      - [rev_split ~max:2 ~sep:"." "toto.mli" = ["toto"; "mli"]]
  *)

val replace : t -> patt : t -> repl : t -> t
  (** [replace text ~patt ~repl] replace all occurences of [patt] in
      [text] by [repl].

      For example:

      - [replace "abcd" ~patt:"b" ~repl:"x" = "axcd"]
      - [replace "Hello world!" ~patt:"world" ~repl:"you" = "Hello you!"]
  *)

(** {6 Tests} *)

val contains : t -> t -> bool
  (** [contains text sub] returns whether [sub] appears in [text] *)

val starts_with : t -> t -> bool
  (** [starts_with text prefix] returns [true] iff [s] starts with
      [prefix].

      For example:

      - [starts_with "abcd" "ab" = true]
      - [starts_with "abcd" "af" = false]
      - [starts_with "ab" "abcd" = false]
  *)

val ends_with : t -> t -> bool
  (** [ends_with s suffix] returns [true] iff [s] ends with
      [suffix].

      For example:

      - [ends_with "abcd" "cd" = true]
      - [ends_with "abcd" "hd" = false]
      - [ends_with "ab" "abc" = false]
  *)

(** {6 Stripping} *)

val strip : ?chars : t list -> t -> t
  (** [strip ?chars text] removes all characters of [text] which are
      part of [chars] at the right and left. [chars] defaults to
      whitespaces. *)

val rstrip : ?chars : t list -> t -> t
  (** [rstrip ?chars text] removes all characters of [text] which are
      part of [chars] at the right. *)

val lstrip : ?chars : t list -> t -> t
  (** [lstrip ?chars text] removes all characters of [text] which are
      part of [chars] at the left. *)

val rchop : t -> t
  (** [rchop t] returns [t] without is last character. Returns [""] if
      [t = ""]. *)

val lchop : t -> t
  (** [lchop t] returns [t] without is first character. Returns [""]
      if [t = ""] *)

(** {6 Pointers} *)

(** Since characters are not encoded by a fixed number of bytes,
    accessing them by character position is not efficient. The
    following functions allow you to iterates in a string in an
    efficient way. *)

type pointer
  (** A pointer to a unicode character in a text. *)

val pointer_l : t -> pointer
  (** Returns a pointer to the left of the given text *)

val pointer_r : t -> pointer
  (** Returns a pointer to the right of the given text *)

val pointer_at : t -> int -> pointer
  (** [pointer_at txt n] returns a pointer to the character at
      position [n] in [txt]. *)

val next : pointer -> (t * pointer) option
  (** [next ptr] if [ptr] is at the end of text, returns [None],
      otherwise, returns [Some(ch, ptr')] where [ch] is the character
      at current position and [ptr'] is the pointer to the next
      character of the text. *)

val prev : pointer -> (t * pointer) option
  (** [prev ptr] if [ptr] is at the beginning of text, returns [None],
      otherwise, returns [Some(ch, ptr')] where [ptr'] points to the
      previous character and [ch] is the character at [ptr']. *)

val move : int -> pointer -> pointer
  (** [move n ptr] moves [ptr] by [n] unicode characters. If [n < 0]
      then [ptr] is moved to the left. Raises [Invalid_argument] if
      the result is outside the text. *)

val chunk : pointer -> pointer -> t
  (** [chunk a b] returns the chunk of text between [a] and
      [b]. Raises [Invalid_arugment] if [a] or [b]. *)

val offset : pointer -> int
  (** [offset ptr] returns the position in bytes of [ptr] *)

val position : pointer -> int
  (** [position ptr] returns the position in unicode character of [ptr] *)

val equal_at : pointer -> t -> bool
  (** [equal_at ptr str] returns wether [ptr] points to a substring
      equal to [str] *)

val find : ?from : pointer -> t -> t -> pointer option
  (** [find ?from text patt] returns a pointer to the first occurrence
      of [patt] in [text]. *)

val rev_find : ?from : pointer -> t -> t -> pointer option
  (** [find ?from text patt] returns a pointer to the last occurrence
      of [patt] in [text]. *)
