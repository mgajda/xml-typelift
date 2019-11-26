#!/bin/bash


stack build -j2

for i in test/simple.xsd test/test.xsd test/person.xsd test/XMLSchema-cleaned.xsd ; do
    stack run xml-typelift-cli -- ${i} > /dev/null 2> /dev/null || (echo "Failed at $i");
done 
