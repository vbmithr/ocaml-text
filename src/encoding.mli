(*
 * encoding.mli
 * ------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

(** {6 Character encodings} *)

type t = string
    (** Type of a character encoding *)

val equal : t -> t -> bool
  (** [equal e1 e2] returns whether [e1] and [e2] denotes the same
      encoding. It does a caseless comparison of [e1] or [e2] without
      optionnal suffixes ("//IGNORE" or "//TRANSLIT"). *)

val system : t
  (** The character encoding used by the system *)

type code_point = int
    (** Type of a unicode code-point. *)

val recode_string : src : t -> dst : t -> string -> string
  (** [recode_string ~src ~dst str] recode [str] from [src] encoding
      to [dst] encoding. *)

(** {6 Decoding} *)

type decoder
  (** Type of a decoder *)

val decoder : t -> decoder
  (** Creates a decoder from an encoding-name *)

(** Result of a decoding operation *)
type decoding_result =
  | Dec_ok of code_point * int
      (** [Dec_ok(code, num)] means that the operation succeed. [code]
          is the unicode code-point read and [num] is the number of
          bytes read by the decoder. *)
  | Dec_need_more
      (** [Dec_not_finished] means that the input contains a
          not-terminated sequence *)
  | Dec_error
      (** [Dec_error] means that the input contains an invalid sequence *)

val decode : decoder -> string -> int -> int -> decoding_result
  (** [decode decoder buffer ptr len] decodes with [decoder] bytes at
      position [ptr] in [buffer] *)

(** {6 Encoding} *)

type encoder
  (** Type of an encoder *)

val encoder : t -> encoder
  (** Creates an encoder from an encoding-name *)

(** Result of an encoding operation *)
type encoding_result =
  | Enc_ok of int
      (** [Enc_ok num] means that the operation succeed and [num]
          bytes have been written. *)
  | Enc_need_more
      (** [Enc_need_more] means that there is not enough space in the
          output to ouput all bytes. *)
  | Enc_error
      (** [Enc_error] means that the given code-point cannot be
          encoded in the given encoding *)

val encode : encoder -> string -> int -> int -> code_point -> encoding_result
  (** [encode decoder buffer ptr len code] encodes [code] with
      [encoder] at position [ptr] in [buffer] *)
