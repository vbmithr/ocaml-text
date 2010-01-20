(*
 * pa_text_env.mli
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

(** Environment *)

include Map.S with type key = string

val lookup : string -> 'a t -> 'a option
  (** [lookup key env] returns the binding of [key] in [env] if
      any. *)
