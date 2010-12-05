(*
 * check_iconv.ml
 * --------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 *)

(** Test whether linking with iconv recquires -liconv *)

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

let ocamlc = ref "ocamlc"
let ext_obj = ref ".o"
let exec_name = ref "a.out"

let log_file = ref ""

(* Search for iconv.h in standard directories. *)
let c_args =
  let rec loop = function
    | [] ->
        ""
    | dir :: dirs ->
        if Sys.file_exists (dir ^ "/include/iconv.h") then
          Printf.sprintf "-ccopt -I%s/include -cclib -L%s/lib" dir dir
        else
          loop dirs
  in
  loop ["/usr"; "/usr/local"]

let compile stub_file caml_file args =
  Printf.ksprintf
    Sys.command
    "%s %s -custom %s %s %s 2> %s"
    !ocamlc args c_args
    (Filename.quote stub_file)
    (Filename.quote caml_file)
    (Filename.quote !log_file)
  = 0

let write_result result =
  let oc = open_out "need_liconv" in
  Printf.fprintf oc "%B" result;
  close_out oc

let safe_remove file_name =
  try
    Sys.remove file_name
  with exn ->
    ()

let () =
  let args = [
    "-ocamlc", Arg.Set_string ocamlc, "<path> ocamlc";
    "-ext_obj", Arg.Set_string ext_obj, "<ext> C object files extension";
    "-exec_name", Arg.Set_string exec_name, "<name> name of the executable produced by ocamlc";
  ] in
  Arg.parse args ignore "check for the need of -liconv\noptions are:";

  prerr_endline "testing whether -liconv is needed";

  (* Put the code into a temporary file. *)
  let stub_file, oc = Filename.open_temp_file "ocaml_text_stub" ".c" in
  output_string oc stub_code;
  close_out oc;

  let caml_file, oc = Filename.open_temp_file "ocaml_text_caml" ".ml" in
  output_string oc caml_code;
  close_out oc;

  log_file := Filename.temp_file "ocaml_text" ".log";

  (* Cleanup things on exit. *)
  at_exit (fun () ->
             safe_remove !log_file;
             safe_remove stub_file;
             safe_remove (Filename.chop_extension (Filename.basename stub_file) ^ !ext_obj);
             safe_remove !exec_name;
             safe_remove caml_file;
             safe_remove (Filename.chop_extension caml_file ^ ".cmi");
             safe_remove (Filename.chop_extension caml_file ^ ".cmo"));

  (* Compile it without -liconv. *)
  if compile stub_file caml_file "" then begin
    prerr_endline "result: -liconv is not needed";
    write_result false
  end else if compile stub_file caml_file "-cclib -liconv" then begin
    prerr_endline "result: -liconv is needed";
    write_result true
  end else begin
    prerr_endline "libiconv seems to be missing!";
    exit 1
  end
