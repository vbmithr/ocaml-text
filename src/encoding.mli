(*
 * encoding.mli
 * ------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

(** Encoding library *)

(** {6 Errors} *)

exception Cannot_encode of string
  (** Exception raised when a character cannot be encoded *)

exception Cannot_decode of string
  (** Exception raised when a character cannot be decoded *)

(** {6 Encoding names} *)

type name = string
    (** Name of an encoding *)

val normalize : name -> name
  (** Normalize an encoding name:

      - lowercase-it
      - replace group of special characters by a single underscore
      - strip spaces and special characters at the begining and at the end

      Special characters are characters other than numbers, letters,
      ['.'] and ['-'].

      For example:

      {[
        normalize "  UTF-8  " = "utf-8"
        normalize "_a_%b$" = "a_b"
      ]}
  *)

(** {6 Encodings representation} *)

(** Encoding try to be as generic as possible. In particular it is
    usable when using monadic IO, like in Lwt. To achieve this, all IO
    operations are written using continuation-passing-style. *)

type 'a put_char = char -> (unit -> 'a) -> 'a
  (** Type of a put_char function. It outputs the given character,
      then call the given continuation. *)

type 'a get_char = (char -> 'a) -> 'a
  (** Type of a get_char function. It consumes one character from the
      input and pass it to the continuation. *)

(** Type of an encoding *)
type t = {
  name : name;
  (** Name of the encoding *)

  min_size : int;
  max_size : int;
  (** Minimum and maximum size taken by an encoded code point. This is
      used for optimization purpose. *)

  encode : 'a. (unit -> 'a) -> (exn -> 'a) -> 'a put_char -> int -> 'a;
  (** [encode k fail put_char code] musts encode one character then
      call the continuation [k] or fail with [fail].

      /!\ WARNING /!\
      It is strictly forbidden to raise an exception in [encode],
      [fail] must be used instead.  *)

  decode : 'a. (int -> 'a) -> (exn -> 'a) -> 'a get_char -> 'a;
  (** [decode k fail get_char] musts decode one character then pass it
      to the continuation [k] or fail with [fail]

      /!\ WARNING /!\
      It is strictly forbidden to raise an exception in [decode],
      [fail] must be used instead.  *)
}

val name : t -> name
val min_size : t -> int
val max_size : t -> int
  (** Projections *)

val encode : t -> (char -> unit) -> int -> unit
  (** [encode enc put_char code] is a short-hand for:

      {[
        enc.encode
          (fun x -> x)
          Pervasives.raise
          (fun ch f -> put_char ch; f ())
          code
      ]}
  *)

val decode : t -> (unit -> char) -> int
  (** [encode enc get_char] is a short-hand for:

      {[
        enc.decode
          (fun x -> x)
          Pervasives.raise
          (fun f -> f (get_char ()))
      ]}
  *)

val of_decoding_table : name : name -> table : int array -> t
  (** [of_decoding_table ~name ~table] creates an encoding from a
      decoding table.

      [table] must be an array of length 256 which maps the caracter
      [ch] to the code-point [table.(Char.code ch)]. *)

(** {6 Predefined encodings} *)

val ascii : t
val utf8 : t
val latin1 : t

(** {6 Mapping name -> encoding} *)

val register : t -> unit
  (** Register the given encoding *)

val lookup : name -> t option
  (** [lookup name] try to find an encoding with name [name]. [name]
      is first normalized with {!normalize}. It also accept
      aliases. *)

val get : name -> t
  (** Same as [lookup] but raises [Not_found] if no encoding can be
      found. *)

(** {6 Predefined encoders/decoders} *)

val recode : ?fallback : char -> src : t -> dst : t -> string -> string
  (** [recode_string ?fallback ~src ~dst str] decode [str] with encoding [src]
      and rencode it with encoding [dst].

      If [fallback] is specified then it is used as a fallback
      character for code-point that cannot be encoded with [dst]. It
      should probably be an ascii character (like '?'). *)

val encode_stream : t -> int Stream.t -> char Stream.t
  (** [encode_stream enc stream] transforms the stream of code-points
      [stream] into a stream of characaters using encoding [enc] *)

val decode_stream : t -> char Stream.t -> int Stream.t
  (** [decode_stream enc stream] transforms the stream of characters
      [stream] into a stream of code-points using encoding [enc] *)

val recode_stream : src : t -> dst : t -> char Stream.t -> char Stream.t
  (** [recode_stream ~src ~dst stream] short-hand for:

      {[
        encode_stream dst (decode_stream src stream)
      ]}
  *)
