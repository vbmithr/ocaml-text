(*
 * enc_string.mli
 * --------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

(** Manipulation of encoded strings *)

(** Note: all functions of this module raises
    {!Encoding.Cannot_decode} if the given string is not encoded in
    the given encoding *)

val length : Encoding.t -> string -> int
  (** [length enc str] returns the number of code-points contained in
      [str] *)

val get : Encoding.t -> string -> int -> int
  (** [get enc str] returns the nth code-point contained in [str] *)

val sub : Encoding.t -> string -> int -> int -> string
  (** [sub enc str ofs len] same as [String.sub] but for encoded
      strings. *)

val check : Encoding.t -> string -> string option
  (** [check enc str] returns:

      - [None] if [str] is encoded with encoding [enc]
      - [Some error_message] otherwise *)

val validate : Encoding.t -> string -> unit
  (** [validate enc str] same as [check] but raises
      {!Encoding.Cannot_decode} if [str] is not encoded with [enc] *)

(** Same functions but without the encoding argument *)
module type S = sig
  val length : string -> int
  val get : string -> int -> int
  val sub : string -> int -> int -> string
  val check : string -> string option
  val validate : string -> unit
end

module Make(E : sig val encoding : Encoding.t end) : S
