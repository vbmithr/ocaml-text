(*
 * setup.ml
 * --------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

(* OASIS_START *)
(* DO NOT EDIT (digest: 7f47a529f70709161149c201ccd90f0b) *)
#use "topfind";;
#require "oasis.dynrun";;
open OASISDynRun;;
(* OASIS_STOP *)

(* List of paths to search for iconv *)
let search_paths = [
  "/usr";
  "/usr/local";
  "/opt";
  "/opt/local";
  "/sw";
  "/mingw";
  "/mingw/local";
]

(* +-----------------------------------------------------------------+
   | Search for iconv.h                                              |
   +-----------------------------------------------------------------+ *)

let search_iconv () =
  let rec loop = function
    | [] ->
        ""
    | dir :: dirs ->
        if Sys.file_exists (dir ^ "/include/iconv.h") then
          dir
        else
          loop dirs
  in
  loop search_paths

let iconv_prefix =
  BaseEnv.var_define
    ~short_desc:(fun () -> "iconv installation prefix")
    "iconv_prefix"
    search_iconv

(* +-----------------------------------------------------------------+
   | Test whether -liconv is needed or not                           |
   +-----------------------------------------------------------------+ *)

let stub_code = "
#include <iconv.h>
#include <caml/mlvalues.h>

CAMLprim value ocaml_text_test()
{
  iconv_open(0, 0);
  return Val_unit;
}
"

let caml_code = "
external test : unit -> unit = \"ocaml_text_test\"
let () = test ()
"

let compile ocamlc log_file stub_file caml_file args =
  let result = ref false and dir = iconv_prefix () in
  OASISExec.run
    ~ctxt:(!OASISContext.default)
    ~f_exit_code:(fun x -> result := x = 0)
    ocamlc
    (List.flatten [
       ["-custom"];
       (if dir = "" then
          []
        else
          ["-ccopt"; "-I" ^ dir ^ "/include";
           "-cclib"; "-L" ^ dir ^ "/lib"]);
       args;
       [Filename.quote stub_file; Filename.quote caml_file; "2>"; Filename.quote log_file];
     ]);
  !result

let safe_remove file_name =
  try
    Sys.remove file_name
  with exn ->
    ()

let printf level msg =
  !(OASISContext.default).OASISContext.printf level msg

let check_iconv () =
  printf `Info "Testing whether -liconv is needed";

  let ocamlc = BaseEnv.var_get "ocamlc"
  and ext_obj = BaseEnv.var_get "ext_obj"
  and exec_name = BaseEnv.var_get "default_executable_name" in

  (* Put the code into a temporary file. *)
  let stub_file, oc = Filename.open_temp_file "ocaml_text_stub" ".c" in
  output_string oc stub_code;
  close_out oc;

  let caml_file, oc = Filename.open_temp_file "ocaml_text_caml" ".ml" in
  output_string oc caml_code;
  close_out oc;

  let log_file = Filename.temp_file "ocaml_text" ".log" in

  (* Cleanup things on exit. *)
  at_exit (fun () ->
             safe_remove log_file;
             safe_remove stub_file;
             safe_remove (Filename.chop_extension (Filename.basename stub_file) ^ ext_obj);
             safe_remove exec_name;
             safe_remove caml_file;
             safe_remove (Filename.chop_extension caml_file ^ ".cmi");
             safe_remove (Filename.chop_extension caml_file ^ ".cmo"));

  (* Compile it without -liconv. *)
  if compile ocamlc log_file stub_file caml_file [] then
    "false"
  else if compile ocamlc log_file stub_file caml_file ["-cclib"; "-liconv"] then
    "true"
  else begin
    printf `Error "libiconv seems to be missing!";
    exit 1
  end

(* Define the need_liconv variable *)
let need_liconv =
  BaseEnv.var_define
    ~short_desc:(fun () -> "-liconv is needed")
    "need_liconv"
    check_iconv

let () = setup ()
