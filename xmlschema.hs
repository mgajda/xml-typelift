{-# LANGUAGE DuplicateRecordFields #-}

import Data.Text

data XsSchema = XsSchema [ XsElement ] deriving Show

data XsElement = SimpleElt XsSimpleType | ComplexElt XsComplexType deriving Show

data XsSimpleType = XsSimpleType { 
    id :: Maybe Text
  , name :: Maybe Text
  , flatType :: Text
  } deriving Show

data XsComplexType = XsComplexType { 
    id :: Maybe Text
  , name :: Maybe Text
  , abstract :: Maybe Bool 
  , mixed :: Maybe Bool 
  , content :: Maybe Content
  } deriving Show

data Content = 
    Simple XsSimpleContent 
  | Complex XsComplexContent 
  | Group XsGroup | All XsAll | Choice XsChoice | Sequence XsSequence
  deriving Show

data XsSimpleContent = SimpleContent {
    id :: Maybe Text
  , kind :: Kind
  } deriving Show  

data Kind = XSrestriction | XsExtension deriving (Eq, Show)

data XsComplexContent = ComplexContent {
    id :: Maybe Text
  , mixed :: Maybe Bool
  } deriving Show

data XsGroup = XsGroup {
    id :: Maybe Text
  , name :: Maybe Text
  , maxOccurs :: Maybe Int -- Use Nothing for unbounded. Default should be 1.
  , minOccurs :: Int
  , elements :: [ XsGroupElt ]
  } deriving Show

data XsGroupElt =
  GroupEltAll XsAll | GroupEltChoice XsChoice | GroupEltSequence XsSequence
  deriving Show

data XsAll =  XsAll {
    id :: Maybe Text
  , minOccurs :: Int -- maxOccurs is 1. No need to specify
  , elements :: [ XsElement ]
  } deriving Show

data XsChoice  = XsChoice {
    id :: Maybe Text
  , maxOccurs :: Maybe Int -- Use Nothing for unbounded. Default should be 1.
  , minOccurs :: Int
  , elements :: [ XsChoiceElt ]
  } deriving Show

data XsChoiceElt =
  ChoiceEltElement XsElement | ChoiceEltGroup XsGroup | ChoiceEltChoice XsChoice |
  ChoiceEltSequence XsSequence | ChoiceEltAny XsAny deriving Show

data XsAny = XsAny {
    id :: Maybe Text
  , maxOccurs :: Maybe Int -- Use Nothing for unbounded. Default should be 1.
  , minOccurs :: Int
  } deriving Show

data XsSequence = XsSequence {
    id :: Maybe Text
  , maxOccurs :: Maybe Int -- Use Nothing for unbounded. Default should be 1.
  , minOccurs :: Int
  , elements :: [ XsSequenceElt ]
  } deriving Show

data XsSequenceElt =
  SequenceEltElement XsElement | SequenceEltGroup XsGroup | SequenceEltChoice XsChoice |
  SequenceEltSequence XsSequence | SequenceEltAny XsAny deriving Show
