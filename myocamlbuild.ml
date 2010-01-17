(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

open Printf
open Ocamlbuild_plugin

(* +-----------------------------------------------------------------+
   | Configuration                                                   |
   +-----------------------------------------------------------------+ *)

let try_exec command =
  try
    let () = Command.execute ~quiet:true (Cmd(S[Sh command; Sh"> /dev/null"; Sh"2> /dev/null"])) in
    true
  with _ ->
    false

let () =
  if not (try_exec "ocamlfind printconf") then begin
    prerr_endline "ocamlfind is not available, please install it";
    exit 1
  end

let have_native = try_exec "ocamlfind ocamlopt -version"
let have_pcre = try_exec "ocamlfind query pcre"

let () =
  let yes_no = function true -> "yes" | false -> "no" in
  printf "\
+--[ compilation options ]----------+
| native compilation:           %3s |
| pcre support:                 %3s |
+-----------------------------------+
%!" (yes_no have_native)
    (yes_no have_pcre)

(* +-----------------------------------------------------------------+
   | Ocamlfind                                                       |
   +-----------------------------------------------------------------+ *)

(* Put here packages you may use in this project: *)
let packages = [
  "type-conv";
  "type-conv.syntax";
  "camlp4";
  "camlp4.extend";
  "camlp4.lib";
  "camlp4.macro";
  "camlp4.quotations.o";
  "camlp4.quotations.r";
  "lwt";
  "lwt.unix";
  "lwt.syntax";
  "lwt.text";
  "str";
  "xmlm";
  "react";
  "irc.bot";
  "text";
  "pcre";
]

(* List of available syntaxes :*)
let syntaxes = [
  "camlp4o";
  "camlp4r";
]

(* +-----------------------------------------------------------------+
   | Utils                                                           |
   +-----------------------------------------------------------------+ *)

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
    | Before_options ->

        (* override default commands by ocamlfind ones *)
        let ocamlfind x = S[A"ocamlfind"; A x] in
        Options.ocamlc   := ocamlfind "ocamlc";
        Options.ocamlopt := ocamlfind "ocamlopt";
        Options.ocamldep := ocamlfind "ocamldep";
        (* FIXME: sometimes ocamldoc say that elements are not found
           even if they are present: *)
        Options.ocamldoc := S[A"ocamlfind"; A"ocamldoc"; A"-hide-warnings"]

    | After_rules ->

        (* +---------------------------------------------------------+
           | Virtual targets                                         |
           +---------------------------------------------------------+ *)

        let virtual_rule name deps =
          rule name ~stamp:name ~deps (fun _ _ -> Nop)
        in

        let libs = if have_pcre then ["text"; "text_pcre"] else ["text"] in

        let byte = List.map (sprintf "src/%s.cma") libs in
        let byte = if have_pcre then "syntax/pa_text_pcre.cmo" :: byte else byte in
        let native = List.map (sprintf "src/%s.cmxa") libs @ List.map (sprintf "src/%s.cmxs") libs in
        let common = ["META"; "text.docdir/index.html"] in

        virtual_rule "all" & byte @ (if have_native then native else []) @ common;
        virtual_rule "byte" & byte @ common;
        virtual_rule "native" & native @ common;

        (* +---------------------------------------------------------+
           | Ocamlfind stuff                                         |
           +---------------------------------------------------------+ *)

        (* When one link an OCaml binary, one should use -linkpkg *)
        flag ["ocaml"; "link"; "program"] & A"-linkpkg";

        (* For each ocamlfind package one inject the -package option
           when compiling, computing dependencies, generating
           documentation and linking. *)
        List.iter
          (fun package -> flag_all_stages ("pkg_" ^ package) (S[A"-package"; A package]))
          packages;

        (* Like -package but for extensions syntax. Morover -syntax is
           useless when linking. *)
        List.iter
          (fun syntax -> flag_all_stages_except_link ("syntax_" ^ syntax) (S[A"-syntax"; A syntax]))
          syntaxes;

        (* +---------------------------------------------------------+
           | C stubs                                                 |
           +---------------------------------------------------------+ *)

        dep ["link"; "ocaml"; "use_libtext"] ["src/libtext_stubs.a"];
        flag ["link"; "library"; "ocaml"; "use_libtext"] & S[A"-cclib"; A"-ltext_stubs"];
        flag ["link"; "library"; "ocaml"; "byte"; "use_libtext"] & S[A"-dllib"; A"-ltext_stubs"];

        (* +---------------------------------------------------------+
           | Misc                                                    |
           +---------------------------------------------------------+ *)

        rule "shared libraries (cmxs)"
          ~dep:"%.cmxa" ~prod:"%.cmxs"
          (fun env _ -> Cmd(S[!(Options.ocamlopt); A"-shared"; A"-linkall"; A"-I"; A"src"; A(env "%.cmxa"); A"-o"; A(env "%.cmxs")]));

        rule "META" ~deps:["META.in"; "VERSION"] ~prod:"META"
          (fun _ _ ->
             Echo([substitute [("@VERSION@", get_version ())] (read_file "META.in")], "META"))

    | _ -> ()
  end
