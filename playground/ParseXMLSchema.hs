import Data.Char
import Data.List
import Text.HTML.TagSoup

capitalize :: String -> String
capitalize s = (toUpper (head s)) : (tail s)

isSimpleType t = (fromAttrib "type" t) /= ""

fromXmlFlatType s = case s of
  "xs:string" -> "String"
  "xs:decimal" -> "Integer"
  "xs:positiveInteger" -> "Int"
  "xs:float" -> "Float"
  "xs:double" -> "Double"
  otherwise -> "String"

simpleType :: Tag String -> (String, String)
simpleType e = 
    (typeName, fromXmlFlatType flat)
   where
     typeName = capitalize $ fromAttrib "name" e
     flat = fromAttrib "type" e

dropUntilOpenTag str = dropWhile (not . (isTagOpenName str))

takeUntilCloseTag str = takeWhile (not . (isTagCloseName str))

-- This doesn't work when there is a nested elements.
-- Need to take until MATCHING closing tag.
extractElt [] = []
extractElt tags =
  let
    firstElt = 
      if (isSimpleType (head tags)) then
        [head tags]
      else
        takeUntilCloseTag "xs:element" tags in
  firstElt:(extractElt (dropUntilOpenTag "xs:element" $ tail tags)) 

-- This doesn't work when there is a nested sequence.
-- Need to take until MATCHING closing tag
extractSeq :: [ Tag String ] -> [ [Tag String] ]
extractSeq tags =
  let 
    str = "xs:sequence"
    seq = takeUntilCloseTag str $ tail $ dropUntilOpenTag str tags in
  extractElt $ dropUntilOpenTag "xs:element" seq

genTypesHelper :: [Tag String] -> (String, String, String, Bool)
genTypesHelper tags = 
  if (isSimpleType openEltTag) then
    let
      typeName = capitalize eltName
      typeStr = fromXmlFlatType $ fromAttrib "type" openEltTag in
    ("newtype " ++ typeName ++ " = " ++ typeStr, eltName, typeStr, False)
  else
    let
      c = dropWhile (not . (isTagOpenName "xs:complexType")) $ tail t
      openCpxTag = head c
      isMixed = (fromAttrib "mixed" openCpxTag) == "true" in
    if (isMixed) then
      ("Mixed type.  Don't know how to do this yet", "null", "null", True)
    else
      let
        elements = extractSeq $ tail c
        typeStrTriples = map genTypesHelper elements
        complexTypeStrs = filter (\(_,_,_, isComplex) -> isComplex) typeStrTriples
        depTypeDefStr = intercalate "\n\n" $ map (\(t,_,_,_) -> t) complexTypeStrs
        innerTypeStrs = map (\(_,elt,typ,_) -> elt ++ " :: " ++ typ) typeStrTriples
        depTypes = intercalate "\n  , " innerTypeStrs in
      (depTypeDefStr ++ "\n\ndata " ++ typeName ++ " = " ++ typeName ++ " {\n    " 
        ++ depTypes ++ "\n}\n" , eltName, typeName, True)
  where
    openEltTag = head tags
    eltName = fromAttrib "name" openEltTag
    t = tail tags
    typeName = capitalize $ fromAttrib "name" openEltTag

genTypes::String -> String
genTypes s = 
  let (str,_,_,_) = genTypesHelper $ dropWhile (not . (isTagOpenName "xs:element")) $ parseTags s in
  str


simpleXML = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">\n<xs:element name=\"shiporder\" type=\"xs:float\">\n</xs:element>\n</xs:schema>"

personXML = "<xs:schema ...>\n<xs:element name=\"person\">\n<xs:complexType mixed=\"false\">\n<xs:sequence>\n <xs:element name=\"name\" type=\"xs:string\">\n<xs:element name=\"age\"  type=\"xs:positiveInteger\">\n</xs:sequence>\n</xs:complexType>\n</xs:element>\n</xs:schema ...>"

simple = genTypes simpleXML

complex = genTypes personXML

main = do
  xml <- readFile "person.xsd"
  putStrLn $ genTypes xml
