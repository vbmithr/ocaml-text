(*
 * encoding.ml
 * -----------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

type t = string
type code_point = int
type decoder
type encoder
type decoding_result =
  | Dec_ok of code_point * int
  | Dec_need_more
  | Dec_error
type encoding_result =
  | Enc_ok of int
  | Enc_need_more
  | Enc_error

external init : unit -> t = "ml_text_init"
external decoder : t -> decoder = "ml_text_decoder"
external encoder : t -> encoder = "ml_text_encoder"
external stub_decode : decoder -> string -> int -> int -> decoding_result = "ml_text_decode"
external stub_encode : encoder -> string -> int -> int -> code_point -> encoding_result = "ml_text_encode"
external stub_recode_string : t -> t -> string -> string = "ml_text_recode_string"

let system = init ()

let decode decoder buf pos len =
  if pos < 0 || pos + len > String.length buf then
    invalid_arg "Encoding.decode"
  else
    stub_decode decoder buf pos len

let encode decoder buf pos len code =
  if pos < 0 || pos + len > String.length buf then
    invalid_arg "Encoding.encode"
  else
    stub_encode decoder buf pos len code

let equal a b =
  let len_a = String.length a and len_b = String.length b in
  let rec loop i =
    let end_of_a = i = len_a || (i + 2 <= len_a && a.[i] = '/' && a.[i + 1] = '/')
    and end_of_b = i = len_b || (i + 2 <= len_b && b.[i] = '/' && b.[i + 1] = '/') in
    if end_of_a && end_of_b then
      true
    else if end_of_a || end_of_b then
      false
    else if Char.lowercase a.[i] = Char.lowercase b.[i] then
      loop (i + 1)
    else
      false
  in
  loop 0

let recode_string ~src ~dst str =
  if equal src dst then
    str
  else
    stub_recode_string src dst str
