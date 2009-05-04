(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

open Printf
open Ocamlbuild_plugin

let flag_all_stages_except_link tag f =
  flag ["ocaml"; "compile"; tag] f;
  flag ["ocaml"; "ocamldep"; tag] f;
  flag ["ocaml"; "doc"; tag] f

let flag_all_stages tag f =
  flag_all_stages_except_link tag f;
  flag ["ocaml"; "link"; tag] f

let substitute env text =
  List.fold_left (fun text (patt, repl) -> String.subst patt repl text) text env

let get_version _ =
  match string_list_of_file "VERSION" with
    | version :: _ -> version
    | _ -> failwith "invalid VERSION file"

let _ =
  dispatch begin function
    | After_rules ->

        (* C stubs: *)
        dep ["link"; "ocaml"; "use_libtext"] ["src/libtext_stubs.a"];
        flag ["link"; "library"; "ocaml"; "use_libtext"] & S[A"-cclib"; A"-ltext_stubs"];
        flag ["link"; "library"; "ocaml"; "byte"; "use_libtext"] & S[A"-dllib"; A"-ltext_stubs"];

        (* Generation of "META" *)
        rule "META" ~deps:["META.in"; "VERSION"] ~prod:"META"
          (fun _ _ ->
             Echo([substitute [("@VERSION@", get_version ())] (read_file "META.in")], "META"))

    | _ -> ()
  end
