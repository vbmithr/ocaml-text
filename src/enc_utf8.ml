(*
 * enc_utf8.ml
 * -----------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of encoding.
 *)

include Enc_string.Make(struct let encoding = Encoding.utf8 end)
