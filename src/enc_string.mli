(*
 * enc_string.mli
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

(** Manipulation of encoded strings *)

(** Note: all functions of this module raises [Encoding.Cannot_decode]
    if the given string is not encoded in the given encoding. *)

type code_point = int
    (** A code point *)

type offset = int
    (** An offset in bytes *)

type position = int
    (** A character position (in code-point) *)

(** {6 Equivalent of latin1 functions} *)

val length : Encoding.t -> string -> int
  (** [length enc str] returns the number of code-points contained in
      [str] *)

val get : Encoding.t -> string -> position -> code_point
  (** [get enc str n] returns the [n]-th code-point contained in [str] *)

val sub : Encoding.t -> string -> position -> int -> string
  (** [sub enc str n len] returns the sub-string starting at character
      postion [n] (in code-point) and of length (in code-point)
      [len] *)

val iter : Encoding.t -> (code_point -> unit) -> string -> unit
  (** [iter enc f str] same as [String.iter] but for encoded
      strings. *)

val iteri : Encoding.t -> (position -> code_point -> unit) -> string -> unit
  (** [iteri enc f str] same as [iter] but also pass the character
      position (in code-point) to [f] *)

(** {6 Buffers, iterators} *)

val add_buffer : Encoding.t -> Buffer.t -> code_point -> unit
  (** [add_buffer enc buf code] add the code-point [code] to [buf] *)

type iterator = offset ref

val next : Encoding.t -> string -> iterator -> code_point
  (** [next enc str iterator] returns the code-point located at offset
      [!ierator] in [str] and advance [iterator] to the offset of the
      next code-point *)

(** {6 Validation} *)

val check : Encoding.t -> string -> string option
  (** [check enc str] returns:

      - [None] if [str] is encoded with encoding [enc]
      - [Some error_message] otherwise *)

val validate : Encoding.t -> string -> unit
  (** [validate enc str] same as [check] but raises
      [Encoding.Cannot_decode] if [str] is not encoded with [enc] *)

(** {6 Misc} *)

val get_offset : Encoding.t -> string -> position -> offset
  (** [get_offset enc str n] returns offset of the [n]-th code-point
      encoded in [str] *)

val get_offset_from : Encoding.t -> string -> offset -> position -> offset
  (** [get_offset enc str ofs n] returns offset of the [n]-th
      code-point encoded in [str] starting at offset [ofs].

      For example:

      {[
        get_offset_from Encoding.ascii "012345" 1 2 = '3'
      ]}
  *)

(** Same functions but without the encoding argument *)
module type S = sig
  val length : string -> int
  val get : string -> position -> code_point
  val sub : string -> position -> int -> string
  val iter : (code_point -> unit) -> string -> unit
  val iteri : (position -> code_point -> unit) -> string -> unit
  val add_buffer : Buffer.t -> code_point -> unit
  val next : string -> iterator -> code_point
  val check : string -> string option
  val validate : string -> unit
  val get_offset : string -> position -> offset
  val get_offset_from : string -> offset -> position -> offset
end

module Make(E : sig val encoding : Encoding.t end) : S
