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
  - [Walid Taha's "Gentle Introduction to Multi-Stage Programming](https://www.cs.tufts.edu/~nr/cs257/archive/walid-taha/dspg04a.pdf)
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

Type names in XML Schema refer to either `xs:complexType` or `xs:simpleType`.
These are either anonymous, if they only occur once, or named, and then they can be re-used by giving
name anywhere in the same schema. Thus empty `xs:complexType` or `xs:simpleType` is usually reference.

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
<xs:schema ...>
  <xs:element name="person">
    <xs:complexType mixed="false">
      <xs:sequence>
        <xs:element name="name" type="xs:string">
        <xs:element name="age"  type="xs:positiveInteger">
      </xs:sequence>
    </xs:complexType>
  </xs:element>
</xs:schema ...>
```

Should be translated to:
```haskell
data Person = Person { name :: String, age :: Int }
```

But with mixed content:
```xml
<xs:element name="p">
  <xs:complexType mixed="true">
    <xs:choice>
      <xs:element name="em"     xs:type="p">
      <xs:element name="strong" xs:type="p">
    <xs:choice>
  </xs:complexType>
</xs:element>
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
but the attribute type can *only* be `xs:simpleType`.
```haskell
data SimpleType = TString | TInteger | ...
```

### Namespaces
* xs:schema xs:targetNamespace="..." gives target namespace for objects.
* this namespace is normally labelled with `xmlns:mynamespace="..."` later - unimportant for us

### A lot of features

There are many more features in XML Schema, that is of *secondary* importance, or can be ignored.
For example `xs:restriction` of existing type - that restricts range of allowed values.
Can be solved by simply assertion on printing the result.

## Glossary
`xs:sequence` - record by element name
`xs:all` - record (the same as `xs:sequence`), but the order of fields may vary (important for decoding only)
`xs:simpleType` - flat type (Int, String etc.)
`xs:complexType` - record type
mixed content - a list of either:
  * any element type
  * text node
`xs:restriction` - restricts type (can be implemented as assertion on output)
