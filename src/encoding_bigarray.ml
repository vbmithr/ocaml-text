(*
 * encoding_bigarray.ml
 * --------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

open Bigarray

type byte_array = (char, int8_unsigned_elt, c_layout) Array1.t

external stub_decode : Encoding.decoder -> byte_array -> int -> int -> Encoding.decoding_result = "ml_text_decode_bigarray"
external stub_encode : Encoding.encoder -> byte_array -> int -> int -> Encoding.code_point -> Encoding.encoding_result = "ml_text_encode_bigarray"

let decode decoder buf pos len =
  if pos < 0 || pos + len > Array1.dim buf then
    invalid_arg "Encoding_bigarray.decode"
  else
    stub_decode decoder buf pos len

let encode decoder buf pos len code =
  if pos < 0 || pos + len > Array1.dim buf then
    invalid_arg "Encoding_bigarray.encode"
  else
    stub_encode decoder buf pos len code
