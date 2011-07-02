(*
 * myocamlbuild.ml
 * ---------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let my_dispatch = function
  | Before_options ->
      Options.make_links := false

  | After_rules ->
      let env = BaseEnvLight.load () in

      if BaseEnvLight.var_get "need_liconv" env = "true" then begin
        flag ["ocamlmklib"; "c"; "use_iconv"] & A"-liconv";
        flag ["link"; "ocaml"; "use_iconv"] & S[A"-cclib"; A"-liconv"]
      end;

      let dir = BaseEnvLight.var_get "iconv_prefix" env in
      if dir <> "" then begin
        flag ["ocamlmklib"; "c"; "use_iconv"] & A("-L" ^ dir ^ "/lib");
        flag ["c"; "compile"; "use_iconv"] & S[A"-ccopt"; A("-I" ^ dir ^ "/include")];
        flag ["link"; "ocaml"; "use_iconv"] & S[A"-cclib"; A("-L" ^ dir ^ "/lib")]
      end

  | _ ->
      ()

let () = dispatch (fun hook -> dispatch_default hook; my_dispatch hook)
