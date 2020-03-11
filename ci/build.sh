#!/bin/bash

source ci/common.sh

# Hpack
message "Run hpack"
hpack
(cd haskell-src-match; hpack)
#cabal v1-install hpack
#hpack

# Build it
message "Build it"

if ghc --numeric-version | grep --quiet "^8.10" ; then
  export XML_TYPELIFT_ADDITIONAL_PACKAGES='scientific pretty-simple'
fi
cabal v2-install --allow-newer --dependencies-only
cabal v2-build   --allow-newer
cabal v2-test    --allow-newer

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
cabal v2-exec -- ghc types.hs  -package iso8601-duration -package xml-typelift -package xeno -package scientific
cabal v2-exec -- ghc parser.hs -package iso8601-duration -package xml-typelift -package xeno -package scientific

# check that benchmarks is working (but limit for 10 minutes only because of slow benchmarking)
message "Benchmarks"
if ghc --numeric-version | grep --quiet "^8.10" ; then
    echo "Benchmarks for 8.10 currently unsupported"
else
    timeout 30m cabal v2-bench generated-parsers-memory
    timeout 30m cabal v2-bench generated-parsers-speed
fi

