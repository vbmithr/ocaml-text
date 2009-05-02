(*
 * iconv.ml
 * --------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
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

external init : unit -> t = "ml_iconv_init"
external decoder : t -> decoder = "ml_iconv_decoder"
external encoder : t -> encoder = "ml_iconv_encoder"
external stub_decode : decoder -> string -> int -> int -> decoding_result = "ml_iconv_decode"
external stub_encode : encoder -> string -> int -> int -> code_point -> encoding_result = "ml_iconv_encode"

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

let rec recode_string ?fallback ~src ~dst str =
  if src = dst then
    str
  else
    let decoder = decoder src and encoder = encoder dst in
    let rec loop_decode fallback src_bytes src_pos dst_bytes dst_pos =
      if src_pos = String.length src_bytes then
        (dst_bytes, dst_pos)
      else
        match decode decoder src_bytes src_pos (String.length src_bytes - src_pos) with
          | Dec_ok(code, count) ->
              loop_encode fallback src_bytes (src_pos + count) dst_bytes dst_pos code
          | Dec_need_more ->
              failwith "Encoding.recode_string: unterminated sequence in input"
          | Dec_error ->
              Printf.ksprintf failwith "Encoding.recode_string: cannot decode sequence at offset %d with %S" src_pos src
    and loop_encode fallback src_bytes src_pos dst_bytes dst_pos code =
      match encode encoder dst_bytes dst_pos (String.length dst_bytes - dst_pos) code with
        | Enc_ok count ->
            loop_decode fallback src_bytes src_pos dst_bytes (dst_pos + count)
        | Enc_need_more ->
            let len = String.length dst_bytes in
            let s = String.create (len * 2) in
            String.blit dst_bytes 0 s 0 dst_pos;
            loop_encode fallback src_bytes src_pos s dst_pos code
        | Enc_error ->
            match fallback with
              | None ->
                  Printf.ksprintf failwith "Encoding.recode_string: cannot encode unicode code-point %04x with %S" code dst
              | Some s ->
                  let dst_bytes, dst_pos = loop_decode None s 0 dst_bytes dst_pos in
                  loop_decode fallback src_bytes src_pos dst_bytes dst_pos
    in
    let len = String.length str in
    let bytes, pos = loop_decode fallback str 0 (String.create len) 0 in
    String.sub bytes 0 pos
