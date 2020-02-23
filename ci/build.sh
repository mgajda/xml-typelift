#!/bin/bash

source ci/common.sh

# Build it
message "Build it"
cabal v1-install --dependencies-only
cabal v1-build   --system-ghc
cabal v1-test    --system-ghc

# check that CLI application is working and output is reasonable
message "Check CLI"
cabal v1-exec -- xml-typelift-cli --version
cabal v1-exec -- xml-typelift-cli --help
cabal v1-exec -- xml-typelift-cli --schema test/data/customersOrders.xsd --types > types.hs
cabal v1-exec -- xml-typelift-cli --schema test/data/customersOrders.xsd         > parser.hs
grep -z "\<data Customers.*= Customers {.*}" types.hs > /dev/null
grep -z "\<parseTopLevelToArray " parser.hs > /dev/null

message "Check generated code compiles"
# TODO: add main action
cabal v1-exec -- ghc types.hs  -o types.o
cabal v1-exec -- ghc parser.hs -o parser.o

# check that benchmarks is working (but limit for 10 minutes only because of slow benchmarking)
message "Benchmarks"
timeout 30m cabal v1-bench xml-typelift

