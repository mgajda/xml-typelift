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


## `xml-typelift-cli` utility

`xml-typelift-cli` is a CLI to XML Typelift functions. It is allow to generate parser and types for XML files by XML Schema.

To build this utility run

```
stack build xml-typelift:exe:xml-typelift-cli
```

You can view supported command line arguments as usual by running `xml-typelift-cli --help` (or using `stack`: `stack run --
`xml-typelift-cli --help`):

```
XML Typelift command line interface

Usage: xml-typelift-cli [--version] --schema FILENAME [--generate-types-only]
  Generates types and parser for XML files by XML schema (.xsd) files

Available options:
  -h,--help                Show this help text
  --version                Show version
  --schema FILENAME        Path to XML schema (.xsd file)
  --generate-types-only    Generate types only
```

So to generate parser for XML Schema `your_schema.xsd` you can run `xml-typelift-cli --schema your_schema.xsd`
and parser and auxiliary types will be output to stdout. Also you can get only types (without parser) by running
`xml-typelift-cli --schema your_schema.xsd --generate-types-only`.

