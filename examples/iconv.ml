(*
 * iconv.ml
 * --------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

(* iconv-like program *)

let src = ref ""
let dst = ref ""

let args = [
  "-f", Arg.Set_string src, "encoding of input";
  "-t", Arg.Set_string dst, "encoding of output";
]

let usage_msg = Printf.sprintf "%s -f <code> -t <code>
Convert encoding of input from one encoding to another.
Options are:" (Filename.basename (Sys.argv.(0)))

let usage _ = Arg.usage args usage_msg; exit 2

let get_encoding name = match Encoding.lookup name with
  | Some enc ->
      enc
  | None ->
      Printf.eprintf "unknown encoding: %S\n%!" name;
      exit 1

let _ =
  Arg.parse args usage usage_msg;

  if !src = "" || !dst = "" then usage ();

  let src = get_encoding !src and dst = get_encoding !dst in
  try
    while true do
      Encoding.encode dst print_char (Encoding.decode src (fun _ -> input_char stdin))
    done
  with
    | End_of_file ->
        ()
