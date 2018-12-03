# Project planning

Initial work is guided by our own schema.

##  Planned features that are still missing to parse it:

1. Handling XML Namespaces in type names to distinguish transunion:date and xsd:date
2. <xs:choice>
3. Nesting of <xs:choice> and <xs:all> or <xs:sequence>
4. <xs:extension> without details
5. <xs:extension> with <xs:complexType>
6. Handling namespaces in Xeno.Node
7. Handling targetNamespace
8. Handling simpleType simpleContent
9. Recognition of special <enumeration> sets (Yes|No, present|absent etc)
10. Handling inclusive/exclusive ranges for integers
11. Implement correct XML Schema types:
      * gYearMonth
      * duration
      * date
      * time
      * zoned time
https://www.w3.org/TR/xmlschema-2/#deviantformats

## Other features that may come in the future

These features do not seem necessary yet:
* xs:element ref
* xs:import and xs:include

These features may be added to Xeno as a part of the project:
* general namespace support
* skipping processing instructions and xml document type declarations


