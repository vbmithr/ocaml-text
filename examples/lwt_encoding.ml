(*
 * lwt_encoding.ml
 * ---------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

open Lwt
open Encoding

let encode enc put_char code =
  enc.encode return fail (fun ch f -> put_char ch >>= f) code

let decode enc get_char =
  enc.decode return fail (fun f -> get_char () >>= f)

let output enc oc code =
  enc.encode return fail (fun ch f -> Lwt_chan.output_char oc >>= f) code

let input enc ic =
  enc.decode return fail (fun f -> Lwt_chan.input_char ic >>= f)
