# Plan of what to do

It would be easiest to start by making simple
[TagSoup](https://hackage.haskell.org/package/tagsoup) script that
generates Haskell data structure representing the data type, and provide
parser/pretty-printer for this structure.

## For further reference

It would be best to use these as reference:
* Great [introduction to XML Schema](https://www.w3schools.com/xml/schema_intro.asp).
* [Zvon reference of XML Schema](http://zvon.org/xxl/xmlSchema2001Reference/Output/Schema/index.html)
* Examples of multi-stage programming:
  - ["Gentle Introduction to Multi-Stage Programming](https://www.cs.tufts.edu/~nr/cs257/archive/walid-taha/dspg04a.pdf" by Walid Tahas)
  - code generation for types is used in `json-autotype:Data/Aeson/AutoType/CodeGen/*.hs`.
* Example schemas with nice explanations:
  - [XML Schema best practices](http://xml.coverpages.org/HP-StephensonSchemaBestPractices.pdf)
  - [ISO-TC211 schema repository](https://github.com/ISO-TC211/XML)
  - [XML Twig repository article](http://www.xmltwig.org/article/bw/bw_04-schema_repositories.html)
  - [Docbook schemas](http://www.oasis-open.org/docbook/xsd/)
  - [OpenGIS schemas](https://github.com/schema-repo/schema-repo)
  - [Schema for XML Schema itself](https://www.w3.org/TR/xmlschema-1/#normative-schemaSchema)

## Basic concepts

### Type names

Type names in XML Schema refer to either `xsd:complexType` or `xsd:simpleType`.
These are either anonymous, if they only occur once, or named, and then they can be re-used by giving
name anywhere in the same schema. Thus empty `xsd:complexType` or `xsd:simpleType` is usually reference.

The entire `XML Schema` will be mapping from type *names* to individual types:
```
type XMLSchema = Map String XMLSchemaType
```

### Mixed types and normal types

The first confusing thing in XML is a distinction between:
* `mixedType=True`  - which is free form mixture of text and elements),
* `mixedType=false` - which works like a record.

Example translation ([without closing brackets, but with indents instead](http://www.iro.umontreal.ca/~lapalme/ForestInsteadOfTheTrees/HTML/ch10s05.html)):
```xml
<?xml version="1.0"?>
<xsd:schema xmlns:xsd="http://www.w3.org/2001/XMLSchema">

  <xsd:element name="person">
    <xsd:complexType mixed="false">
      <xsd:sequence>
        <xsd:element name="name" type="xsd:string" />
        <xsd:element name="age"  type="xsd:positiveInteger" />
        <xsd:element name="birthplace">
          <xsd:complexType mixed="false">
            <xsd:sequence>
              <xsd:element name="city"  type="xsd:string" />
              <xsd:element name="country"  type="xsd:string" />
            </xsd:sequence>
          </xsd:complexType>
        </xsd:element>
      </xsd:sequence>
    </xsd:complexType>
  </xsd:element>

</xsd:schema>
```

Should be translated to:
```haskell
data Birthplace = Birthplace { city :: String, country :: String }

data Person = Person { name :: String, age :: Int, birthplace :: Birthplace }
```

But with mixed content:
```xml
<xsd:element name="p">
  <xsd:complexType mixed="true">
    <xsd:choice>
      <xsd:element name="em"     xsd:type="p" />
      <xsd:element name="strong" xsd:type="p" />
    </xsd:choice>
  </xsd:complexType>
</xsd:element>
```
With example document like:
```xml
<p>Alphabetic <em>or</em> possibly <strong>phonetic</strong> representation.</p>
```


Should be translated into:
```haskell
data P = [PElt]

data PElt = Em     EmT
          | Strong StrongT
          | Text

document = P [Text "Alphabetic", Em [Text "or"], Text " possibly ", Strong [Text "phonetic"], Text " representation"]
-- In this case:
-- type EmT     = [PElt]
-- type StrongT = [PElt]
```
Remember: either `complexType`, and `simpleType` *may* be named for future reference in the same document.

### Elements versus attributes

Attributes are always *attached* to their elements.
Attributes should thus be treated as a flat field in the complexType record,
but the attribute type can *only* be `xsd:simpleType`.
```haskell
data SimpleType = TString | TInteger | ...
```

### Dictionaries
A lot of objects can have unique name that can be referenced for sharing afterwards.
These should ideally be expressed either as same type for each occurence,
or as some kind of type class (for attribute groups)
This applies to:
* `xsd:element`,
* `xsd:simpleType`,
* `xsd:complexType`,
* `xsd:attribute`,
* `xsd:attributeGroup`.

### Namespaces
* xsd:schema xsd:targetNamespace="..." gives target namespace for objects.
* this namespace is normally labelled with `xmlns:mynamespace="..."` later - unimportant for us

### A lot of features

There are many more features in XML Schema, that is of *secondary* importance, or can be ignored.
For example `xsd:restriction` of existing type - that restricts range of allowed values.
Can be solved by simply assertion on printing the result.

## Glossary
`xsd:sequence` - record by element name
`xsd:all` - record (the same as `xsd:sequence`), but the order of fields may vary (important for decoding only)
`xsd:simpleType` - flat type (Int, String etc.)
`xsd:complexType` - record type
mixed content - a list of either:
  * any element type
  * text node
`xsd:restriction` - restricts type (can be implemented as assertion on output)
