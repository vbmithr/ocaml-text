(*
 * encoding_table.mli
 * ------------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

(** Efficient encoding table *)

(** Encoding table are used for charmaps encodings, i.e. ones that use
    256 characters mapped into code-points, like iso8859-* encodings.

    The decoding table of such encodings is an array of 256
    code-points. The encoding table is a mapping from these
    code-points to bytes.

    This module implement efficient (in terms of space and performace)
    table for these encodings. *)

type t
  (** Type of a static encoding table *)

val of_decoding_table : int array -> t
  (** [of_decoding_table table] creates an encoding table from a
      decoding table *)

val of_assoc : (int * char) list -> t
  (** [of_assoc l] transforms the encoding table [l] given as an
      associative list to an efficient encoding table. *)

val lookup : int -> t -> char option
  (** [lookup code table] returns the value associated to [code] in
      [table]. Returns [None] if [code] is not found in [table].

      This a [O(ln(cardinal(map)))] operation. *)
