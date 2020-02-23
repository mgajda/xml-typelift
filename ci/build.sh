#!/bin/bash

source ci/common.sh

# Hpack
#message "Run hpack"
#cabal v1-install hpack
#hpack

# Build it
message "Build it"
cabal v2-install --dependencies-only --allow-newer
cabal v2-build   --system-ghc        --allow-newer
cabal v2-test    --system-ghc        --allow-newer

# check that CLI application is working and output is reasonable
message "Check CLI"
cabal v2-exec -- xml-typelift-cli --version
cabal v2-exec -- xml-typelift-cli --help
cabal v2-exec -- xml-typelift-cli --schema test/data/customersOrders.xsd --types > types.hs
cabal v2-exec -- xml-typelift-cli --schema test/data/customersOrders.xsd         > parser.hs
grep -z "\<data Customers.*= Customers {.*}" types.hs > /dev/null
grep -z "\<parseTopLevelToArray " parser.hs > /dev/null

message "Check generated code compiles"
# TODO: add main action
cabal v2-exec -- ghc types.hs  -o types.o
cabal v2-exec -- ghc parser.hs -o parser.o

# check that benchmarks is working (but limit for 10 minutes only because of slow benchmarking)
message "Benchmarks"
timeout 30m cabal v2-bench xml-typelift

