#!/usr/bin/env python

# dump_python_aliases.py
# ----------------------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of encoding.

import sys, os, encodings

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

aliases = {}
aliases["latin_1"] = "iso8859_1"
for (k, v) in encodings.aliases.aliases.iteritems():
    k = hard_normalize(k)
    v = hard_normalize(v)
    if v == "latin_1":
        v = "iso8859_1"
    if k != v:
        aliases[k] = v

items = aliases.items()
items.sort()

f.write("""
(* File generated with %(program_name)s *)

let aliases = Hashtbl.create %(count)d

let _ =
  List.iter (fun (alias, name) -> Hashtbl.add aliases alias name) [
""" % { "program_name" : os.path.basename(sys.argv[0]),
        "count" : len(items) })

for k, v in items:
    f.write("""    ("%s", "%s");\n""" % (k, v))

f.write("  ]\n")
f.close()
