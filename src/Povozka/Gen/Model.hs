module Povozka.Gen.Model where

import Data.Text qualified as T
import Data.Word (Word32)

type TypeName = T.Text
type VarName = T.Text

data Field
  = Conditional !VarName (Maybe Int) !TypeName
  | Field !TypeName

data Combinator = Combinator
  { constr :: !TypeName
  , constrId :: !Word32
  , typeName :: !TypeName
  , fields :: ![(VarName, Field)]
  }
