/*
 * ml_text.c
 * ---------
 * Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 */

#define _ISOC99_SOURCE
#include <errno.h>
#include <iconv.h>
#include <langinfo.h>
#include <locale.h>
#include <string.h>
#include <strings.h>
#include <wctype.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>

/* define the easiest encoding to use: */
#ifdef ARCH_BIG_ENDIAN
#define NATIVE_UCS "UCS-4BE"
#else
#define NATIVE_UCS "UCS-4LE"
#endif

/* Constant for ocaml constructors: */
#define NEED_MORE (Val_int(0))
#define ERROR (Val_int(1))

/* +------------------------------------+
   | Custom block for iconv descriptors |
   +------------------------------------+ */

#define Iconv_val(v) (*(iconv_t*)Data_custom_val(v))

void ml_iconv_finalize(value cd)
{
  iconv_close(Iconv_val(cd));
}

int ml_iconv_compare(value v1, value v2)
{
  return (int)((long)Iconv_val(v1) - (long)Iconv_val(v2));
}

long ml_iconv_hash(value v)
{
  return (long)Iconv_val(v);
}

static struct custom_operations ops = {
  "iconv",
  ml_iconv_finalize,
  ml_iconv_compare,
  ml_iconv_hash,
  custom_serialize_default,
  custom_deserialize_default
};

/* +----------------+
   | Initialization |
   +----------------+ */

/* This function returns the system encoding: */
CAMLprim value ml_iconv_init(value unit)
{
  CAMLparam1(unit);
  /* Set the locale acording to environment variables: */
  setlocale(LC_CTYPE, "");
  setlocale(LC_COLLATE, "");
  /* Get the codeset used by current locale: */
  char *codeset = nl_langinfo(CODESET);
  /* If the encoding cannot be determined, just use ascii: */
  CAMLreturn(caml_copy_string(codeset ? codeset : "ASCII"));
}

/* +----------+
   | Decoding |
   +----------+ */

CAMLprim value ml_iconv_decoder(value enc)
{
  CAMLparam1(enc);

  /* A decoder is an iconv descriptor from enc to UCS-4: */
  iconv_t cd = iconv_open(NATIVE_UCS, String_val(enc));

  if (cd == (iconv_t)-1)
    caml_failwith("Encoding.decoder: invalid encoding");
  else
    {
      value result = caml_alloc_custom(&ops, sizeof(iconv_t), 0, 1);
      *(iconv_t*) Data_custom_val(result) = cd;
      CAMLreturn(result);
    }
}

CAMLprim value ml_iconv_decode(value cd_val, value buf_val, value pos_val, value len_val)
{
  CAMLparam4(cd_val, buf_val, pos_val, len_val);

  uint32 code;
  size_t len = Int_val(len_val);
  size_t in_left = len;
  char *in_bytes = String_val(buf_val) + Int_val(pos_val);
  size_t out_left = 4;
  char *out_bytes = (char*)&code;

  iconv(Iconv_val(cd_val), &in_bytes, &in_left, &out_bytes, &out_left);

  if (out_left == 0) {
    value result = caml_alloc_tuple(2);
    Store_field(result, 0, Val_int(code));
    Store_field(result, 1, Val_int(len - in_left));
    CAMLreturn(result);
  } else if (errno == EINVAL)
    CAMLreturn(NEED_MORE);
  else
    CAMLreturn(ERROR);
}

/* +----------+
   | Encoding |
   +----------+ */

CAMLprim value ml_iconv_encoder(value enc)
{
  CAMLparam1(enc);

  /* A decoder is an iconv descriptor from UCS-4 to enc: */
  iconv_t cd = iconv_open(String_val(enc), NATIVE_UCS);

  if (cd == (iconv_t)-1)
    caml_failwith("Encoding.encoder: invalid encoding");
  else
    {
      value result = caml_alloc_custom(&ops, sizeof(iconv_t), 0, 1);
      *(iconv_t*) Data_custom_val(result) = cd;
      CAMLreturn(result);
    }
}

CAMLprim value ml_iconv_encode(value cd_val, value buf_val, value pos_val, value len_val, value code_val)
{
  CAMLparam5(cd_val, buf_val, pos_val, len_val, code_val);

  uint32 code = Int_val(code_val);
  size_t len = Int_val(len_val);
  size_t in_left = 4;
  char *in_bytes = (char*)&code;
  size_t out_left = len;
  char *out_bytes = String_val(buf_val) + Int_val(pos_val);

  iconv(Iconv_val(cd_val), &in_bytes, &in_left, &out_bytes, &out_left);

  if (in_left == 0) {
    value result = caml_alloc_tuple(1);
    Store_field(result, 0, Val_int(len - out_left));
    CAMLreturn(result);
  } else if (errno == E2BIG)
    CAMLreturn(NEED_MORE);
  else
    CAMLreturn(ERROR);
}

/* +---------------------+
   | Character utilities |
   +---------------------+ */

value ml_text_upper(value ch) {
  return Val_int(towupper(Int_val(ch)));
}

value ml_text_lower(value ch) {
  return Val_int(towlower(Int_val(ch)));
}

#define IS(name) value ml_text_is_##name(value ch) { return Val_bool(isw##name(Int_val(ch))); }

IS(alnum)
IS(alpha)
IS(blank)
IS(cntrl)
IS(digit)
IS(graph)
IS(lower)
IS(print)
IS(punct)
IS(space)
IS(upper)

/* +-----------------+
   | Text comparison |
   +-----------------+ */

CAMLprim value ml_text_compare(value s1, value s2) {
  CAMLparam2(s1, s2);
  int res = strcoll(String_val(s1), String_val(s2));
  if (res < 0)
    CAMLreturn(Val_int(-1));
  else if (res > 0)
    CAMLreturn(Val_int(1));
  else
    CAMLreturn(Val_int(0));
}
