#!/bin/sh
set -e
set -x

# Install local dependencies
# opam install -y --deps-only --with-test --locked ./ligo.opam

# NEW-PROTOCOL-TEMPORARY
opam install -y --deps-only --locked ./ligo.opam || true
opam install bisect_ppx
# NEW-PROTOCOL-TEMPORARY
