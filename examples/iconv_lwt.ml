(*
 * iconv_lwt.ml
 * ------------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

(* iconv-like program, with lwt *)

open Lwt

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

let rec loop src dst ic oc =
  Lwt_encoding.input src ic >>= fun code ->
    Lwt_encoding.output dst oc code >>= fun _ ->
      loop src dst ic oc

let _ =
  Arg.parse args usage usage_msg;

  if !src = "" || !dst = "" then usage ();

  let src = get_encoding !src and dst = get_encoding !dst in
  let ic = Lwt_chan.in_channel_of_descr (Lwt_unix.of_unix_file_descr Unix.stdin)
  and oc = Lwt_chan.out_channel_of_descr (Lwt_unix.of_unix_file_descr Unix.stdout) in
  Lwt_unix.run
    (catch
       (fun _ ->
          loop src dst ic oc)
       (function
          | End_of_file ->
              Lwt_chan.flush oc >>= fun _ -> Lwt_chan.close_out oc
          | exn -> fail exn))
