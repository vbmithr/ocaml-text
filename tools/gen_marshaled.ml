(*
 * gen_marshaled.ml
 * ----------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

open Printf

let _ =
  let oc =
    if Array.length Sys.argv = 2 then
      open_out Sys.argv.(1)
    else
      stdout
  in

  fprintf oc "
(* File generated with %s *)

let encodings = [
" (Filename.basename Sys.argv.(0));

  List.iter begin fun (real_name, hard_name, table) ->
    fprintf oc "    (%S, %S, %S);\n" real_name hard_name (Marshal.to_string table [])
  end Encodings_generated.encodings;

  fprintf oc "  ]\n";
  close_out oc

