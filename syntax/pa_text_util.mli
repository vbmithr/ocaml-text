(*
 * pa_text_util.mli
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

(** Utilities *)

val split_hexa_quotation : Text.t list -> (Text.t * Text.t list) option
  (** Recognises patterns of the form "{XXX}" at the beginning of a
      string. If it match, it returns the quotation and the rest of
      the list. *)
