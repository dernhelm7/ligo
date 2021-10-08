#!/bin/sh

# TODO this is exactly like install_vendors_deps.sh but doesn't
# install the vendored libs

set -e
set -x

# Install local dependencies
export PATH=~/.cargo/bin:$PATH
# NEW-PROTOCOL-TEMPORARY
# opam install -y --deps-only --with-test --locked=locked ./ligo.opam
opam install -y --deps-only --locked ./ligo.opam || true
opam install -y bisect_ppx
# NEW-PROTOCOL-TEMPORARY