#!/bin/bash

stack build --copy-bins &&

xml-typelift-cli ../tuxml/tuxml_schema-883.xsd > /tmp/latest.hs &&

stack exec -- ghc /tmp/latest.hs &&

grep WARNING /tmp/latest.hs | sort | uniq -c
