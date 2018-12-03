# Converting XML Schema to Haskell datatype

GitLab build: [![pipeline status](https://gitlab.com/migamake/xml-typelift/badges/master/pipeline.svg)](https://gitlab.com/migamake/xml-typelift/commits/master)


Travis CI: [![Travis (.org)](https://img.shields.io/travis/mgajda/xml-typelift.svg)](https://travis-ci.org/mgajda/xml-typelift)


XML TypeLift will allow to use XML Schema to create Haskell data type and parser for it.

That will allow us to easily handle large XML Schemas like Office OpenXML made by Microsoft.

It is part of DataHaskell initiative on type providers in Haskell.

## Team

* [Michal](http://github.com/mgajda) - works on this
* [Kevin](http://github.com/dataopt) - made initial explorations in the playground/, now retired from the project.

## How?

* XML Schema parser:
  - Xeno.DOM - is much faster than other XML parsers, and pure Haskell
    * we added enhanced error reporting as part of the project
    * we plan to add the following too:
      - namespace support
      - XML fragment parsing
* We will use Haskell code generation like [`json-autotype`](http://github.com/mgajda/json-autotype)
* Sample schemas will be put into `test/` directory

