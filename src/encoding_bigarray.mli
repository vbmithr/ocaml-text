(*
 * encoding_bigarray.mli
 * ---------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

(** Encoding/decoding to/from bigarrays *)

open Bigarray

type byte_array = (char, int8_unsigned_elt, c_layout) Array1.t
  (** Type of array of bytes. *)

val decode : Encoding.decoder -> byte_array -> int -> int -> Encoding.decoding_result
val encode : Encoding.encoder -> byte_array -> int -> int -> Encoding.code_point -> Encoding.encoding_result
