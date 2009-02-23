(*
 * lwt_encoding.mli
 * ----------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

(** Encoding support for Lwt *)

val encode : Encoding.t -> (char -> unit Lwt.t) -> int -> unit Lwt.t
  (** [encode enc put_char] create a code-point writer from the given
      encoding and put_char function. *)

val decode : Encoding.t -> (unit -> char Lwt.t) -> int Lwt.t
  (** [decode enc get_char] decode a code-point using the givne
      encoding and [get_char] function *)

val output : Encoding.t -> Lwt_chan.out_channel -> int -> unit Lwt.t
  (** [output enc oc code] output a code point encoded in [enc] on
      [oc] *)

val input : Encoding.t -> Lwt_chan.in_channel -> int Lwt.t
  (** [input enc ic] decode a code point from [ic] using [enc] *)
