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

-- Extract a beginning xml fragment enclosed in tagName 
-- Returns the fragment and the unconsumed portion
fragmentHelper :: String -> Int -> ([Tag String], [Tag String]) -> ( [Tag String], [Tag String])
fragmentHelper tagName level (frag, [] ) = (frag, [])
fragmentHelper tagName level (frag, rest) =
  let 
    (h:t) = rest
    h' = (h:frag) in
  if (isTagOpenName tagName h) then
    let newLevel = level + if (isSimpleType h) then 0 else 1 in
    fragmentHelper tagName (newLevel) (h', t)
  else
    if (isTagCloseName tagName h) then
      if (level == 1) then
        (h', t)
      else 
        fragmentHelper tagName (level-1) (h', t)
    else
      fragmentHelper tagName level (h', t)

fragment tagName tags =
  let t = dropUntilOpenTag tagName tags 
      t' = fragmentHelper tagName 0 ([], t) in
  (reverse $ fst t', snd t')
  

extractElts [] = []
extractElts tags =
  let t = dropUntilOpenTag "xs:element" tags in
  case t of
    [] -> []
    otherwise ->
      let
        splitElt = if (isSimpleType (head t)) then
                     ([head t], tail t)
                   else
                     fragment "xs:element" tags in
      (fst splitElt): extractElts (snd splitElt) 

extractSeq :: [ Tag String ] -> [ [Tag String] ]
extractSeq tags = 
  extractElts $ fst $ fragment "xs:sequence" tags

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
      (depTypeDefStr ++ "data " ++ typeName ++ " = " ++ typeName ++ " {\n    " 
        ++ depTypes ++ "\n}\n\n" , eltName, typeName, True)
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

testXML = "<xs:sequence name=\"seq\"><xs:element name\"elt1\">hello</xs:element><xs:sequence name=\"seq2\"></xs:sequence>blah</xs:sequence>"

main = do
  putStrLn "Simple type: "
  putStrLn $ simple
  putStrLn "\n\nComplex type: "
  putStrLn $ complex
  putStrLn "\n\nFrom file person.xsd: "
  xml <- readFile "person.xsd"
  putStrLn $ genTypes xml
