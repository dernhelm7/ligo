#!/bin/sh
set -e
set -x

# NEW-PROTOCOL-TEMPORARY
cd ./vendors
if [ ! -d "./tezos" ]
then git clone git@gitlab.com:tezos/tezos.git ./tezos;
fi
cd ./tezos
git fetch
git checkout v11.0-rc1
git submodule init
cd ../..
# NEW-PROTOCOL-TEMPORARY

# Install local dependencies
# opam install -y --deps-only --with-test --locked ./ligo.opam

# NEW-PROTOCOL-TEMPORARY
opam install -y --deps-only --locked ./ligo.opam || true
opam install bisect_ppx
# NEW-PROTOCOL-TEMPORARY
