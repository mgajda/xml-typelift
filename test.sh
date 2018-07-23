#!/bin/bash


for i in test/simple.xsd test/test.xsd test/person.xsd test/XMLSchema.xsd test/bad.xsd; do
  stack exec xml-typelift ${i} || (echo "Failed at $i"; exit 1);
done 
