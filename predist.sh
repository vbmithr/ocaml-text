#!/bin/sh

# Generate CHANGES.darcs
[ -d "$DARCS_REPO" ] && darcs changes --repodir "$DARCS_REPO" > CHANGES.darcs

# Build the user manual for release
cd manual
make manual.pdf
# Remove intermediate files
make clean-aux
cd ..

# Add oasis stuff:
OASIS setup

# Cleanup
rm -f predist.sh make-dist.sh
