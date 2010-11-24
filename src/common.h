/*
 * common.h
 * --------
 * Copyright : (c) 2010, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of ocaml-text.
 */

#ifndef __COMMON_H
#define __COMMON_H

#include <caml/config.h>
#include <caml/mlvalues.h>

/* define the easiest encoding to use: */
#ifdef ARCH_BIG_ENDIAN
#define NATIVE_UCS "UCS-4BE"
#else
#define NATIVE_UCS "UCS-4LE"
#endif

/* Constant for ocaml constructors: */
#define Val_need_more (Val_int(0))
#define Val_error (Val_int(1))

#define Iconv_val(v) (*(iconv_t*)Data_custom_val(v))

#endif /* __COMMON_H */
