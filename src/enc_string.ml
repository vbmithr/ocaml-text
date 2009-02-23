(*
 * enc_string.ml
 * -------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

open Encoding

type offset = int
type position = int
type code_point = int
type iterator = offset ref

let next_char pos str =
  let i = !pos in
  if i >= String.length str then
    raise (Cannot_decode "premature end of string")
  else begin
    pos := i + 1;
    str.[i]
  end

let next enc str pos =
  let ofs = !pos in
  if ofs < 0 || ofs >= String.length str then invalid_arg "Enc_string.next";
  decode enc (fun _ -> next_char pos str)

let length enc str =
  let len = String.length str in
  if enc.min_size = enc.max_size then begin
    let size = enc.min_size in
    if len mod size <> 0 then
      (* Raise the same exception than for variable-length
         encodings *)
      raise (Cannot_decode "premature end of string")
    else
      len / size
  end else begin
    let pos = ref 0 and l = ref 0 in
    while !pos < len do
      ignore (decode enc (fun _ -> next_char pos str));
      incr l
    done;
    !l
  end

let get_offset_from enc str ofs n =
  if n < 0 || ofs < 0 || ofs >= String.length str then invalid_arg "Enc_string.get_offset_from";
  if enc.min_size = enc.max_size then
    ofs + n * enc.min_size
  else begin
    let pos = ref ofs in
    (* Skip (n-1) characters *)
    for i = 1 to n do
      if !pos = String.length str then invalid_arg "Enc_string.get_offset_from";
      ignore (decode enc (fun _ -> next_char pos str))
    done;
    !pos
  end

let get_offset enc str n = get_offset_from enc str 0 n

let get enc str n =
  let ofs = get_offset enc str n in
  if ofs >= String.length str then invalid_arg "Enc_string.get";
  let pos = ref ofs in
  decode enc (fun _ -> next_char pos str)

let sub enc str n len =
  let ofs_start = get_offset enc str n in
  let ofs_end = get_offset_from enc str ofs_start len in
  String.sub str ofs_start (ofs_end - ofs_start)

let iter enc f str =
  let pos = ref 0 in
  while !pos < String.length str do
    f (decode enc (fun _ -> next_char pos str))
  done

let iteri enc f str =
  let pos = ref 0 and n = ref 0 in
  while !pos < String.length str do
    f !n (decode enc (fun _ -> next_char pos str));
    incr n
  done

let add_buffer enc buf code = encode enc (Buffer.add_char buf) code

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

module Make(E : sig val encoding : Encoding.t end) : S =
struct
  open E
  let length = length encoding
  let get = get encoding
  let sub = sub encoding
  let iter = iter encoding
  let iteri = iteri encoding
  let add_buffer = add_buffer encoding
  let next = next encoding
  let check = check encoding
  let validate = validate encoding
  let get_offset = get_offset encoding
  let get_offset_from = get_offset_from encoding
end
