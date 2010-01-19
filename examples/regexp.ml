(*
 * regexp.ml
 * ---------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

let digit3 = <:re< ["0".."9"]{1-3} >>

let () =
  match Sys.argv with
    | [|program_name; <:re< (digit3 as d1 : int)
                        "." (digit3 as d2 : int)
                        "." (digit3 as d3 : int)
                        "." (digit3 as d4 : int) >>|] ->
        Printf.printf "d1 = %d, d2 = %d, d3 = %d, d4 = %d\n" d1 d2 d3 d4
    | _ ->
        Printf.printf "usage: %s <ipv4 address>\n" Sys.argv.(0)
