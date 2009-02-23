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

(* +-----------------------------------+
   | Packages installed with ocamlfind |
   +-----------------------------------+ *)

(* List of used packages *)
let packages = [ "camlp4";
                 "camlp4.lib";
                 "camlp4.macro";
                 "camlp4.quotations.o";
                 "camlp4.quotations.r";
                 "dynlink";
                 "lwt" ]

(* List of syntaxes *)
let syntaxes = ["camlp4o"; "camlp4r"]

(* +-------+
   | Utils |
   +-------+ *)

let flag_all_stages_except_link tag f =
  flag ["ocaml"; "compile"; tag] f;
  flag ["ocaml"; "ocamldep"; tag] f;
  flag ["ocaml"; "doc"; tag] f

let flag_all_stages tag f =
  flag_all_stages_except_link tag f;
  flag ["ocaml"; "link"; tag] f

let define_lib ?dir name =
  ocaml_lib ?dir name;
  dep ["ocaml"; "byte"; "use_" ^ name] [name ^ ".cma"];
  dep ["ocaml"; "native"; "use_" ^ name] [name ^ ".cmxa"]

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
        Options.ocamldoc := ocamlfind "ocamldoc"

    | After_rules ->
        Pathname.define_context "tools" [ "src" ];

        define_lib ~dir:"src" "encoding";

        (* Generation of encodings and aliases. This is only needed
           for the development version *)
        List.iter begin fun name ->
          let tool = sprintf "tools/dump_python_%s.py" name
          and fname = sprintf "src/%s_generated.ml" name in
          rule (sprintf "automatic generation of %s" name) ~dep:tool ~prod:fname
            (fun _ _ -> Seq[Cmd(S[A"mkdir"; A"-p"; A"src"]);
                            Cmd(S[A"python"; A tool; A fname])])
        end ["encodings"; "aliases"];

        let tool = "tools/gen_marshaled.byte"
        and fname = "src/marshaled_encodings_generated.ml" in
        rule (sprintf "automatic generation of %s" fname) ~dep:tool ~prod:fname
          (fun _ _ -> Seq[Cmd(S[A"mkdir"; A"-p"; A"src"]);
                          Cmd(S[A"ocamlrun"; A tool; A fname])]);

        (* +-----------------+
           | Ocamlfind stuff |
           +-----------------+ *)

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

        (* +-------+
           | Other |
           +-------+ *)

        (* Generation of "META" *)
        rule "META" ~deps:["META.in"; "VERSION"] ~prod:"META"
          (fun _ _ ->
             Echo([substitute [("@VERSION@", get_version ())] (read_file "META.in")], "META"))

    | _ -> ()
  end
