(*
 * encoding.ml
 * -----------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

open Printf

type name = string

let hlookup table key =
  try
    Some(Hashtbl.find table key)
  with
      Not_found -> None

exception Cannot_encode of string
exception Cannot_decode of string

type 'a put_char = char -> (unit -> 'a) -> 'a
type 'a get_char = (char -> 'a) -> 'a

let is_valid code = code >= 0 && code <= 0x10ffff

let cannot_encode name code =
  if is_valid code then
    Cannot_encode(sprintf "'%s' cannot encode code-point U+%04x" name code)
  else
    Cannot_encode(sprintf "invalid code point: %x" code)

let cannot_decode name byte = Cannot_decode(sprintf "'%s' cannot decode byte 0x%02x" name byte)

type t = {
  name : name;
  min_size : int;
  max_size : int;
  encode : 'a. (unit -> 'a) -> (exn -> 'a) -> 'a put_char -> int -> 'a;
  decode : 'a. (int -> 'a) -> (exn -> 'a) -> 'a get_char -> 'a;
}

external identity : 'a -> 'a = "%identity"

let name enc = enc.name
let min_size enc = enc.min_size
let max_size enc = enc.max_size
let encode enc put_char code = enc.encode identity raise (fun ch f -> put_char ch; f ()) code
let decode enc get_char = enc.decode identity raise (fun f -> f (get_char ()))

let of_decoding_table ~name ~table =
  if Array.length table <> 256 then
    invalid_arg "Encoding.of_decoding_table"
  else
    let encoding_table = Encoding_table.of_decoding_table table in
    { name = name;
      min_size = 1;
      max_size = 1;
      encode = (fun k fail put code ->
                  match Encoding_table.lookup code encoding_table with
                    | None ->
                        fail (cannot_encode name code)
                    | Some ch ->
                        put ch k);
      decode = (fun k fail get -> get (fun ch -> k table.(Char.code ch))) }

(* +------------------------------+
   | Predefined encoders/decoders |
   +------------------------------+ *)

let encode_stream enc stream =
  let pending = Queue.create () in
  Stream.from (fun _ ->
                 if Queue.is_empty pending then begin
                   try
                     encode enc (fun ch -> Queue.push ch pending) (Stream.next stream);
                     if Queue.is_empty pending then
                       None
                     else
                       Some(Queue.pop pending)
                   with
                     | Stream.Failure -> None
                 end else
                   Some(Queue.pop pending))

let decode_stream enc stream =
  Stream.from (fun _ ->
                 try
                   Some(decode enc (fun _ -> Stream.next stream))
                 with
                   | Stream.Failure -> None)

let recode_stream ~src ~dst stream = encode_stream dst (decode_stream src stream)

let next_char pos str =
  let i = !pos in
  if i >= String.length str then
    raise (Cannot_decode "premature end of string")
  else begin
    pos := i + 1;
    str.[i]
  end

let recode ?fallback ~src ~dst str =
  let pos = ref 0 and buf = Buffer.create (String.length str * dst.min_size / src.min_size) in
  begin match fallback with
    | None ->
        while !pos < String.length str do
          encode dst (Buffer.add_char buf) (decode src (fun _ -> next_char pos str))
        done
    | Some fallback ->
        let tmp = Buffer.create dst.max_size in
        while !pos < String.length str do
          let code = decode src (fun _ -> next_char pos str) in
          Buffer.clear tmp;
          try
            encode dst (Buffer.add_char tmp) code;
            Buffer.add_buffer buf tmp
          with
              Cannot_encode _ ->
                Buffer.add_char buf fallback
        done
  end;
  Buffer.contents buf

(* +----------------------+
   | Predefined encodings |
   +----------------------+ *)

let ascii = {
  name = "ascii";
  min_size = 1;
  max_size = 1;
  encode = (fun k fail put code ->
              if code >= 0 && code <= 127 then
                put (Char.unsafe_chr code) k
              else
                fail (cannot_encode "ascii" code));
  decode = (fun k fail get ->
              get (fun ch ->
                     let code = Char.code ch in
                     if code <= 127 then
                       k code
                     else
                       fail (cannot_decode "ascii" code)));
}

let latin1 = {
  name = "iso8859-1";
  min_size = 1;
  max_size = 1;
  encode = (fun k fail put code ->
              if code >= 0 && code <= 255 then
                put (Char.unsafe_chr code) k
              else
                fail (cannot_encode "iso8859-1" code));
  decode = (fun k fail get -> get (fun ch -> k (Char.code ch)));
}

let utf8 = {
  name = "utf-8";
  min_size = 1;
  max_size = 4;
  encode = (fun k fail put code ->
              let put x k = put (Char.unsafe_chr x) k in
              if code < 0 then
                fail (cannot_encode "utf-8" code)
              else if code < 0x80 then
                put code k
              else if code <= 0x800 then
                put ((code lsr 6) lor 0xc0)
                  (fun _ -> put ((code land 0x3f) lor 0x80) k)
              else if code <= 0x10000 then
                put ((code lsr 12) lor 0xe0)
                  (fun _ -> put (((code lsr 6) land 0x3f) lor 0x80)
                     (fun _ -> put ((code land 0x3f) lor 0x80) k))
              else if code <= 0x10ffff then
                put ((code lsr 18) lor 0xf0)
                  (fun _ -> put (((code lsr 12) land 0x3f) lor 0xe0)
                     (fun _ -> put (((code lsr 6) land 0x3f) lor 0x80)
                        (fun _ -> put ((code land 0x3f) lor 0x80) k)))
              else
                fail (cannot_encode "utf-8" code));
  decode = (fun k fail get ->
              let rec trail minimum acc = function
                | 0 ->
                    if acc < minimum then
                      fail (Cannot_decode "overlong utf-8 sequence")
                    else
                      k acc
                | count ->
                    get (fun ch ->
                           let n = Char.code ch in
                           if n land 0xc0 = 0x80 then
                             trail minimum ((acc lsl 6) lor (n land 0x3f)) (count - 1)
                           else
                             fail (Cannot_decode "unterminated utf-8 sequence"))
              in
              get (fun ch ->
                     let n = Char.code ch in
                     if n land 0x80 = 0 then
                       k n
                     else if n land 0xe0 = 0xc0 then
                       trail 0x80 (n land 0x1f) 1
                     else if n land 0xf0 = 0xe0 then
                       trail 0x800 (n land 0x0f) 2
                     else if n land 0xf8 = 0xf0 then
                       trail 0x10000 (n land 0x07) 3
                     else
                       fail (cannot_decode "utf-8" n)));
}

(* +-------+
   | Names |
   +-------+ *)

let aliases = Aliases_generated.aliases

let is_special = function
  | '0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '.' | '-' ->
      false
  | _ ->
      true

let normalize name =
  let buf = Buffer.create (String.length name) in
  let rec loop previous_is_special i =
    if i = String.length name then
      Buffer.contents buf
    else
      let ch = name.[i] in
      if is_special ch then
        loop true (i + 1)
      else begin
        if previous_is_special then Buffer.add_char buf '_';
        Buffer.add_char buf (Char.lowercase ch);
        loop false (i + 1)
      end
  in
  (* Skip special characters at the beginning *)
  let rec start i =
    if i = String.length name then
      ""
    else if is_special name.[i] then
      start (i + 1)
    else
      loop false i
  in
  start 0

(* Hard-normalization: normalizes then replaces everything that is not
   an alpha-numerical character by an underscore in [name]. This is
   used for aliases. *)
let hard_normalize name =
  let name = normalize name in
  for i = 0 to String.length name - 1 do
    let ch = name.[i] in
    match ch with
      | '0' .. '9' | 'a' .. 'z' ->
          ()
      | _ ->
          name.[i] <- '_'
  done;
  name

let resolv name =
  let name = hard_normalize name in
  match hlookup aliases name with
    | Some name -> name
    | None -> name

(* +----------------------+
   | Encodings management |
   +----------------------+ *)

(* Table of registred encodings. It maps hard-normalized names to
   pairs of the form (real-name, lazy-encoding) *)
let registred = Hashtbl.create 16

let _register real_name hard_name lenc =
  Hashtbl.add registred hard_name (name, lenc)

let register enc = _register enc.name (hard_normalize enc.name) (lazy enc)

let _ = List.iter register [ascii; utf8; latin1]

let lookup name =
  match hlookup registred (resolv name) with
    | Some(_, lenc) -> Some(Lazy.force lenc)
    | None -> None

let get name = match lookup name with
  | Some enc -> enc
  | None -> raise Not_found

let _ =
  List.iter
    (fun (real_name, hard_name, decoding_table) ->
       _register real_name hard_name (lazy(of_decoding_table real_name decoding_table)))
    Encodings_generated.encodings
