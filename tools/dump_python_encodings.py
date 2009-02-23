#!/usr/bin/env python

# dump_python_encodings.py
# ------------------------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of encoding.

import sys, os, codecs, encodings

# Same function as the one in src/encoding.ml
def hard_normalize(name):
    l = []
    for c in name:
        if c.isalnum():
            l.append(c)
        else:
            l.append('_')
    return ''.join(l)

if len(sys.argv) == 2:
    f = open(sys.argv[1], "w")
else:
    f = sys.stdout

# This part list python codecs, it is inspired by the script
# Tools/unicode/listcodecs.py in python sources
all_codecs = []
for filename in os.listdir(encodings.__path__[0]):
    if filename[-3:] == '.py':
        name = filename[:-3]
        if name != 'iso8859_1':
            try:
                all_codecs.append((name, codecs.lookup(name)))
            except:
                pass

all_codecs.sort()

f.write("""
(* File generated with %s *)

let encodings = [
""" % os.path.basename(sys.argv[0]))

for name, codec in all_codecs:
    try:
        module = __import__("encodings.%s" % name)
        table = getattr(module, name).decoding_table
    except:
        continue
    f.write("""    ("%s", "%s",\n""" % (codec.name, hard_normalize(codec.name)))
    f.write("""     [|""")
    l = 8
    for i in range(256):
        s = str(ord(table[i]))
        if i < 255: s += ";"
        if 1 + l + len(s) > 80:
            f.write("\n       ")
            l = 8 + len(s)
        else:
            l += len(s)
        f.write(s)
    f.write("|]);\n")

f.write("  ]\n")
