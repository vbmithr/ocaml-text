(*
 * text_pcre.mli
 * -------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

(** This module is used by the syntax extension *)

val exec : Pcre.substrings array ref -> (Pcre.regexp Lazy.t * Text.t) array -> bool
val regexp : string -> Pcre.regexp
val get_substring : Pcre.substrings -> int -> Text.t
val get_substring_ofs : Pcre.substrings -> int -> int
