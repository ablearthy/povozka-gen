{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Povozka.Gen.TH (generateConstructor, generateType, generateTypeFamily, pprint) where

import Language.Haskell.TH

import Data.Text qualified as T
import Povozka.Gen.Model

import Data.Maybe (fromMaybe)

text2name :: T.Text -> Name
text2name = mkName . T.unpack

intermediateTypeToType :: IntermediateType -> Type
intermediateTypeToType (Ap a b) = AppT (intermediateTypeToType a) (intermediateTypeToType b)
intermediateTypeToType (SimpleType t) = ConT $ text2name t

generateConstructor :: Combinator -> Dec
generateConstructor comb =
  DataD
    []
    constrName
    []
    Nothing
    [RecC constrName fields]
    []
  where
    constrName = text2name comb.constr

    fields = map (\(fname, ftype) -> (text2name fname, defaultBang, intermediateTypeToType (tr_ ftype))) comb.fields
      where
        tr_ (Field x) = x
        tr_ (Conditional _ _ x) = x

    defaultBang = Bang NoSourceUnpackedness SourceStrict

generateType :: TypeName -> [Combinator] -> Dec
generateType t combs = DataD [] constrName [] Nothing constructors []
  where
    constrName = text2name t

    strip_prime x = fromMaybe x (T.stripSuffix "'" x)

    constructors = map (\comb -> NormalC (text2name (strip_prime comb.constr)) [(defaultBang, ConT $ text2name comb.constr)]) combs
    defaultBang = Bang NoSourceUnpackedness SourceStrict

generateTypeFamily :: [Combinator] -> [Dec]
generateTypeFamily [] = []
generateTypeFamily combs@(c : _) = a : b
  where
    a = generateType c.typeName combs
    b = map generateConstructor combs
