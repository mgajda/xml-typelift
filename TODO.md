Code cleanups:
* use lens
* make builders fully abstract outside CodeGenMonad and TypeDecl
* abstract newtype generation etc.

Yet unsupported features:
* attribute/element groups
* restrictions other than enumeration and pattern
* namespaces (altough it may be easy to add)
  - this would better be added at Xeno level.
  - that also means that the following are ignored:
    * targetNamespace
    * requiring qualification
    * attributes with same name but wrong namespace are taken
* FromXML should be moved to `xeno`
* keys and uniques
* imports and includes

