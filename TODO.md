Current issue after cleanup:
* "content" types are tagged wrong
* TTopLevel suggests wrong name allocation
* typeAlias instead of newtype for TopLevel
* missing restrictions, and extensions spill over as "content" types
!!! Check for changes and regressions with respect to previous version!!!
* add unit tests for DSLs

Code cleanups:
* use lens
* make builders fully abstract outside CodeGenMonad and TypeDecl
* replace builders with DSL
* abstract newtype generation etc.
* unify declareSumType and declareAlgebraicType
* split TyCtx (with) and Ctx (without type)
* LogicT for choosing common prefix

Yet unsupported features:
* attribute/element groups
* restrictions other than enumeration and pattern
* xeno:
  - dropping doctype
  - namespaces (altough it may be easy to add)
    - this would better be added at Xeno level.
    - that also means that the following are ignored:
      * targetNamespace
      * requiring qualification
      * attributes with same name but wrong namespace are taken
* FromXML should be moved to `xeno`
* keys and uniques
* imports and includes
* <xs:any> element

