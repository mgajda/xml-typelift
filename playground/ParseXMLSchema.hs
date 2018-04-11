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

simpleType e = 
    (typeName, fromXmlFlatType flat)
   where
     typeName = capitalize $ fromAttrib "name" e
     flat = fromAttrib "type" e

dropUntilOpenTag str = dropWhile (not . (isTagOpenName str))

takeUntilCloseTag str = takeWhile (not . (isTagCloseName str))

extractSeq tags =
  let str = "xs:sequence"
      s = takeUntilCloseTag str $ tail $ dropUntilOpenTag str tags in
  filter (isTagOpenName "xs:element") s

complexType typeName c = 
  let openCpxTag = head c
      isMixed = (fromAttrib "mixed" openCpxTag) == "true" in
  case isMixed of
    True -> "Mixed type.  Don't know how to do this yet"
    False -> -- Just a record type
      let elements = extractSeq $ tail c
          nameTypePairs = map simpleType elements
          typeStrs = map (\(t,n) -> t ++ " :: " ++ n) nameTypePairs
          depTypes = concat $ intersperse "\n  , " typeStrs in
      "data " ++ typeName ++ " = " ++ typeName ++ " {\n    " 
              ++ depTypes
              ++ "\n}\n"
      

genTypes::String -> String
genTypes s = 
    let t = dropWhile (not . (isTagOpenName "xs:element")) tags 
        openEltTag = head t
        typeName = capitalize $ fromAttrib "name" openEltTag in
    case isSimpleType openEltTag of
      True -> let (n, typeStr) = simpleType openEltTag in
              "newtype " ++ (capitalize n) ++ " = " ++ typeStr
      False -> complexType typeName $ dropWhile (not . (isTagOpenName "xs:complexType")) $ tail t
  where
    tags = parseTags s


simpleXML = "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n<xs:schema xmlns:xs=\"http://www.w3.org/2001/XMLSchema\">\n<xs:element name=\"shiporder\" type=\"xs:float\">\n</xs:element>\n</xs:schema>"

personXML = "<xs:schema ...>\n<xs:element name=\"person\">\n<xs:complexType mixed=\"false\">\n<xs:sequence>\n <xs:element name=\"name\" type=\"xs:string\">\n<xs:element name=\"age\"  type=\"xs:positiveInteger\">\n</xs:sequence>\n</xs:complexType>\n</xs:element>\n</xs:schema ...>"

simple = genTypes simpleXML

complex = genTypes personXML

main = do
   putStrLn simple
   putStrLn complex
