(*
 * pa_text_types.ml
 * ----------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

(** Common types *)

type mode = Caseless | Multiline | Dot_all
    (** Modes that can be enabled or disabled *)

type greediness = Greedy | Lazy | Possessive
    (** Greediness of a regular expression *)

type direction = Behind | Ahead
    (** Direction of a look-around expression *)
