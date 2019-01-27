#!/bin/bash


stack build -j2

for i in test/simple.xsd test/test.xsd test/person.xsd test/bad.xsd ../tuxml/*.xsd test/XMLSchema-cleaned.xsd ; do
  stack exec xml-typelift-cli ${i} || (echo "Failed at $i"; exit 1);
done 
