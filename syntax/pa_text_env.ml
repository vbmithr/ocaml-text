(*
 * pa_text_env.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

include Map.Make(String)

let lookup key env =
  try Some(find key env) with Not_found -> None
