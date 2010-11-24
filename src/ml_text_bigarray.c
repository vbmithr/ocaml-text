/*
 * ml_text_bigarray.c
 * ------------------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 */

#include <errno.h>
#include <iconv.h>
#include <string.h>

#include <caml/bigarray.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include "common.h"

CAMLprim value ml_text_decode_bigarray(value cd_val, value buf_val, value pos_val, value len_val)
{
  CAMLparam4(cd_val, buf_val, pos_val, len_val);

  uint32 code;
  size_t len = Long_val(len_val);
  size_t in_left = len;
  char *in_bytes = (char*)Caml_ba_data_val(buf_val) + Long_val(pos_val);
  size_t out_left = 4;
  char *out_bytes = (char*)&code;

  iconv(Iconv_val(cd_val), &in_bytes, &in_left, &out_bytes, &out_left);

  if (out_left == 0) {
    value result = caml_alloc_tuple(2);
    Store_field(result, 0, Val_int(code));
    Store_field(result, 1, Val_int(len - in_left));
    CAMLreturn(result);
  } else if (errno == EINVAL)
    CAMLreturn(Val_need_more);
  else
    CAMLreturn(Val_error);
}

CAMLprim value ml_text_encode_bigarray(value cd_val, value buf_val, value pos_val, value len_val, value code_val)
{
  CAMLparam5(cd_val, buf_val, pos_val, len_val, code_val);

  uint32 code = Int_val(code_val);
  size_t len = Long_val(len_val);
  size_t in_left = 4;
  char *in_bytes = (char*)&code;
  size_t out_left = len;
  char *out_bytes = (char*)Caml_ba_data_val(buf_val) + Long_val(pos_val);

  iconv(Iconv_val(cd_val), &in_bytes, &in_left, &out_bytes, &out_left);

  if (in_left == 0) {
    value result = caml_alloc_tuple(1);
    Store_field(result, 0, Val_int(len - out_left));
    CAMLreturn(result);
  } else if (errno == E2BIG)
    CAMLreturn(Val_need_more);
  else
    CAMLreturn(Val_error);
}
