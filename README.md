# Converting XML Schema to Haskell datatype

XML TypeLift will allow to use XML Schema to create Haskell data type and parser for it.

That will allow us to easily handle large XML Schemas like Office OpenXML made by Microsoft.

It is part of DataHaskell initiative on type providers in Haskell.

## Team

* [Michal](http://github.com/mgajda)
* [Kevin](http://github.com/dataopt)

## How?

* XML Schema parser:
  - TagSoup
  - HXT
* We will use Haskell code generation like [`json-autotype`](http://github.com/mgajda/json-autotype)
* Sample schemas will be put into `test/` directory

## Little help from build system

[![Build Status](https://api.travis-ci.org/mgajda/json-autotype.svg?branch=master)](https://travis-ci.org/mgajda/xml-typelift)
