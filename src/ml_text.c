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
#include <locale.h>
#include <string.h>
#include <strings.h>
#include <wctype.h>
#include <stdio.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/fail.h>

/* There is no nl_langinfo on windows: */
#ifdef _WIN32
#include <windows.h>
#else
#include <langinfo.h>
#endif

#include "common.h"

/* +-----------------------------------------------------------------+
   | Custom block for iconv descriptors                              |
   +-----------------------------------------------------------------+ */

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

/* +-----------------------------------------------------------------+
   | Initialization                                                  |
   +-----------------------------------------------------------------+ */

/* This function returns the system encoding: */
CAMLprim value ml_text_init(value unit)
{
  CAMLparam1(unit);
  /* Set the locale acording to environment variables: */
  setlocale(LC_CTYPE, "");
  setlocale(LC_COLLATE, "");
#ifdef _WIN32
  /* Use codepage on windows */
  char codeset[128];
  sprintf(codeset, "CP%d", GetACP());
  CAMLreturn(caml_copy_string(codeset));
#else
  /* Get the codeset used by current locale: */
  char *codeset = nl_langinfo(CODESET);
  /* If the encoding cannot be determined, just use ascii: */
  CAMLreturn(caml_copy_string(codeset ? codeset : "ASCII"));
#endif
}

/* +-----------------------------------------------------------------+
   | Decoding                                                        |
   +-----------------------------------------------------------------+ */

CAMLprim value ml_text_decoder(value enc)
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

CAMLprim value ml_text_decode(value cd_val, value buf_val, value pos_val, value len_val)
{
  CAMLparam4(cd_val, buf_val, pos_val, len_val);

  uint32 code;
  size_t len = Long_val(len_val);
  size_t in_left = len;
  char *in_bytes = String_val(buf_val) + Long_val(pos_val);
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

/* +-----------------------------------------------------------------+
   | Encoding                                                        |
   +-----------------------------------------------------------------+ */

CAMLprim value ml_text_encoder(value enc)
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

CAMLprim value ml_text_encode(value cd_val, value buf_val, value pos_val, value len_val, value code_val)
{
  CAMLparam5(cd_val, buf_val, pos_val, len_val, code_val);

  uint32 code = Int_val(code_val);
  size_t len = Long_val(len_val);
  size_t in_left = 4;
  char *in_bytes = (char*)&code;
  size_t out_left = len;
  char *out_bytes = String_val(buf_val) + Long_val(pos_val);

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

/* +-----------------------------------------------------------------+
   | Character utilities                                             |
   +-----------------------------------------------------------------+ */

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
IS(xdigit)

/* +-----------------------------------------------------------------+
   | Text comparison                                                 |
   +-----------------------------------------------------------------+ */

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

/* +-----------------------------------------------------------------+
   | String recoding                                                 |
   +-----------------------------------------------------------------+ */

CAMLprim value ml_text_recode_string(value enc_src, value enc_dst, value str)
{
  CAMLparam3(str, enc_src, enc_dst);
  CAMLlocal1(result);

  iconv_t cd = iconv_open(String_val(enc_dst), String_val(enc_src));

  if (cd == (iconv_t)-1)
    caml_failwith("Encoding.recode_string: invalid encoding");

  /* Length of the output buffer. It is initialised to the length of
     the input string, which should be a good
     approximation: */
  size_t len = caml_string_length(str);

  /* Pointer to the beginning of the output buffer. The +1 is for the
     NULL terminating byte. */
  char *dst_buffer = malloc(len + 1);

  if (dst_buffer == NULL)
    caml_failwith("Encoding.recode_string: out of memory");

  /* iconv arguments */
  char *src_bytes = String_val(str);
  char *dst_bytes = dst_buffer;
  size_t src_remaining = len;
  size_t dst_remaining = len;

  while (src_remaining) {
    size_t count = iconv (cd, &src_bytes, &src_remaining, &dst_bytes, &dst_remaining);

    if (count == (size_t) -1) {
      switch (errno) {
      case EILSEQ:
        free(dst_buffer);
        iconv_close(cd);
        caml_failwith("Encoding.recode_string: invalid multibyte sequence found in the input");

      case EINVAL:
        free(dst_buffer);
        iconv_close(cd);
        caml_failwith("Encoding.recode_string: incomplete multibyte sequence found in the input");

      case E2BIG: {
        /* Ouput offest relative to the beginning of the destination
           buffer: */
        size_t offset = dst_bytes - dst_buffer;

        /* Try with a buffer 2 times bigger: */
        len *= 2;
        dst_buffer = realloc(dst_buffer, len + 1);
        if (dst_buffer == NULL)
          caml_failwith("Encoding.recode_string: out of memory");

        dst_bytes = dst_buffer + offset;
        dst_remaining += len;
        break;
      }

      default:
        free(dst_buffer);
        iconv_close(cd);
        caml_failwith("Encoding.recode_string: unknown error");
      }
    }
  };

  *dst_bytes = 0;
  result = caml_alloc_string(dst_bytes - dst_buffer);
  memcpy(String_val(result), dst_buffer, dst_bytes - dst_buffer);

  /* Clean-up */
  free(dst_buffer);
  iconv_close(cd);

  CAMLreturn(result);
}

/* +-----------------------------------------------------------------+
   | Text normalization                                              |
   +-----------------------------------------------------------------+ */

CAMLprim value ml_text_strxfrm(value string)
{
  CAMLparam1(string);
  size_t length = strxfrm(NULL, String_val(string), 0);
  char buffer[length + 1];
  strxfrm(buffer, String_val(string), length + 1);
  CAMLreturn(caml_copy_string(buffer));
}
