#!/bin/bash

# size_stats.sh
# -------------
# Copyright : (c) 2009, Jeremie Dimino <jeremie@dimino.org>
# Licence   : BSD3
#
# This file is a part of encoding.

if [ ! -f Makefile ]; then
    echo "You must launch this script in the top directory."
    exit 1
fi

a="src/encodings_generated.native"
b="src/marshaled_encodings_generated.native"
empty="empty.native"

touch empty.ml
ocamlbuild $a $b $empty
rm -f empty.ml

size ()
{
    echo $(cat _build/$1 | wc -c)
}

size_a=$(size $a)
size_b=$(size $b)
size_empty=$(size $empty)

echo
echo "Size of the charmap database in the library:"
echo

printf "without marshaling: %8d KB\n" $(( (size_a - size_empty) / 1024 ))
printf "with marshaling:    %8d KB\n" $(( (size_b - size_empty) / 1024 ))
