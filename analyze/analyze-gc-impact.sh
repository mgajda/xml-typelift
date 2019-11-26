#!/bin/bash -e

## This script run benchmarks with different `+RTS -Hxx` options
## and collect benchmark results in "output.csv"

# Time in seconds per each benchmarks;
# recommended is 60 seconds
TIME_PER_EACH_BENCH="60"

# TODO extract from `stack`
BENCHMARK_FN=".stack-work/dist/x86_64-linux-tinfo6/Cabal-2.4.0.1/build/generated-parsers-speed/generated-parsers-speed"

stack build xml-typelift:bench:generated-parsers-speed --no-run-benchmarks

echo "Name,Mean,MeanLB,MeanUB,Stddev,StddevLB,StddevUB,MB" > output.csv

for i in `seq 0 13`; do
#for i in "1" "13"; do
    mem=`echo "2^$i" | bc`
    memopt="-H${mem}M"
    echo "===== BENCHMARKING WITH $mem ($memopt) ====="
    for benchmark_name in `$BENCHMARK_FN --list` ; do
        [ -e tmp.csv ] && rm tmp.csv
        # NB: Add "-m glob '*/parser*'" to filter benchmarks
        GHCRTS="$memopt" $BENCHMARK_FN $benchmark_name -L$TIME_PER_EACH_BENCH --csv tmp.csv
        cat tmp.csv | awk -F',' "BEGIN { OFS=\",\" } { \$7=\"$mem\"; print}" | tail --lines=+2 >> output.csv
        rm tmp.csv
    done
done
