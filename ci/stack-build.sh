#!/bin/bash

source ci/common.sh

# Build it
message "Build it"
stack install --system-ghc
stack build   --system-ghc
stack test    --system-ghc

# check that CLI application is working and output is reasonable
message "Check CLI"
stack exec -- xml-typelift-cli --version
stack exec -- xml-typelift-cli --help
stack exec -- xml-typelift-cli --schema test/data/customersOrders.xsd --types > types.hs
stack exec -- xml-typelift-cli --schema test/data/customersOrders.xsd > parser.hs
grep -z "\<data Customers.*= Customers {.*}" types.hs > /dev/null
grep -z "\<parseTopLevelToArray " parser.hs > /dev/null

message "Check generated code runs"
stack exec -- ghc types.hs
stack exec -- ghc parser.hs

# check that benchmarks is working (but limit for 10 minutes only because of slow benchmarking)
message "Check benchmarks"
timeout 30m stack bench xml-typelift
