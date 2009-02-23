(*
 * enc_string.ml
 * -------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

open Encoding

let next_char pos str =
  let i = !pos in
  if i >= String.length str then
    raise (Cannot_decode "premature end of string")
  else begin
    pos := i + 1;
    str.[i]
  end

let length enc str =
  let pos = ref 0 and l = ref 0 in
  while !pos < String.length str do
    ignore (decode enc (fun _ -> next_char pos str));
    incr l
  done;
  !l

let get enc str ofs =
  if ofs < 0 then
    raise (Invalid_argument "Enc_string.get")
  else
    let pos = ref 0 in
    for i = 1 to ofs do
      if !pos = String.length str then
        raise (Invalid_argument "Enc_string.get")
      else
        ignore (decode enc (fun _ -> next_char pos str))
    done;
    if !pos = String.length str then
      raise (Invalid_argument "Enc_string.get")
    else
      decode enc (fun _ -> next_char pos str)

let sub enc str ofs len =
  if ofs < 0 || len < 0 then
    raise (Invalid_argument "Enc_string.sub")
  else
    let pos = ref 0 in
    for i = 1 to ofs do
      if !pos = String.length str then
        raise (Invalid_argument "Enc_string.sub")
      else
        ignore (decode enc (fun _ -> next_char pos str))
    done;
    let pos_start = !pos in
    for i = 1 to len do
      if !pos = String.length str then
        raise (Invalid_argument "Enc_string.sub")
      else
        ignore (decode enc (fun _ -> next_char pos str))
    done;
    String.sub str pos_start (!pos - pos_start)

let validate enc str =
  let pos = ref 0 in
  while !pos < String.length str do
    ignore (decode enc (fun _ -> next_char pos str))
  done

let check enc str =
  try
    validate enc str;
    None
  with
      Cannot_decode msg -> Some msg

module type S = sig
  val length : string -> int
  val get : string -> int -> int
  val sub : string -> int -> int -> string
  val check : string -> string option
  val validate : string -> unit
end

module Make(E : sig val encoding : Encoding.t end) : S =
struct
  open E
  let length = length encoding
  let get = get encoding
  let sub = sub encoding
  let check = check encoding
  let validate = validate encoding
end
