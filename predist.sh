#!/bin/sh

# Generate CHANGES.darcs
[ -d "$DARCS_REPO" ] && darcs changes --repodir "$DARCS_REPO" > CHANGES.darcs

# Generate charmaps and aliases
python tools/dump_python_encodings.py > src/encodings_generated.ml
python tools/dump_python_aliases.py > src/aliases_generated.ml

# Remove this file
rm -f predist.sh
