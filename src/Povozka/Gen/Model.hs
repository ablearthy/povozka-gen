module Povozka.Gen.Model where

import Data.Text qualified as T
import Data.Word (Word32)

import Data.Map.Strict qualified as M

type TypeName = T.Text
type VarName = T.Text

type ConstrMap = M.Map TypeName Combinator
type TypeMap = M.Map TypeName ConstrMap

data IntermediateType
  = SimpleType !TypeName
  | Ap IntermediateType IntermediateType
  deriving (Show)

data Field
  = Conditional !VarName (Maybe Int) !IntermediateType
  | Field !IntermediateType
  deriving (Show)

data Combinator = Combinator
  { constr :: !IntermediateType
  , constrId :: !Word32
  , typeName :: !IntermediateType
  , fields :: ![(VarName, Field)]
  }
  deriving (Show)
