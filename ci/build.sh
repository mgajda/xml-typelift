#!/bin/bash

source ci/common.sh

# Build it
message "Build it"
cabal install --dependencies-only
cabal build   --system-ghc
cabal test    --system-ghc

# check that CLI application is working and output is reasonable
message "Check CLI"
cabal exec -- xml-typelift-cli --version
cabal exec -- xml-typelift-cli --help
cabal exec -- xml-typelift-cli --schema test/data/customersOrders.xsd --types > types.hs
cabal exec -- xml-typelift-cli --schema test/data/customersOrders.xsd         > parser.hs
grep -z "\<data Customers.*= Customers {.*}" types.hs > /dev/null
grep -z "\<parseTopLevelToArray " parser.hs > /dev/null

message "Check generated code compiles"
# TODO: add main action
cabal exec -- ghc types.hs  -o types.o
cabal exec -- ghc parser.hs -o parser.o

# check that benchmarks is working (but limit for 10 minutes only because of slow benchmarking)
message "Benchmarks"
timeout 30m cabal bench xml-typelift

