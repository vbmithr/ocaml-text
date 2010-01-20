(*
 * pa_text_util.ml
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

let split_hexa_quotation = function
  | "{" :: x :: l when Text.is_xdigit x ->
      let rec skip_hexa acc = function
        | "}" :: l ->
            Some(Text.rev_implode ("}" :: acc), l)
        | x :: l when Text.is_xdigit x ->
            skip_hexa (x :: acc) l
        | _ ->
            None
      in
      skip_hexa [x; "{"] l
  | _ ->
      None
