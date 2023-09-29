{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Povozka.Gen.TH (generateConstructor, generateType, generateTypeFamily, generateBinaryInstanceForConstructor, pprint, runQ) where

import Language.Haskell.TH

import Data.Text qualified as T
import Povozka.Gen.Model

import Data.Maybe (fromMaybe)
import Data.List (foldl')
import Data.Map.Strict qualified as M

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

    fields = map (\(fname, ftype) -> (text2name fname, defaultBang, (tr_ ftype))) comb.fields
      where
        tr_ (Field x) = intermediateTypeToType x
        tr_ (Conditional _ _ x) = AppT (ConT $ mkName "Maybe") $ intermediateTypeToType x

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

generateBinaryInstanceForConstructor :: Combinator -> Q Dec
generateBinaryInstanceForConstructor comb = do
  g <- bodyGet
  pure $ InstanceD Nothing [] instanceName [FunD (mkName "get") [Clause [] (NormalB g) []]]
  where
    instanceName = AppT (ConT (mkName "Data.Binary.Binary")) (ConT combName)
    combName = text2name comb.constr

    bodyGet = do
        (names, stmts) <- generateGetParams comb.fields
        thePure <- runQ [|pure|]
        let r = foldl' AppE (ConE combName) names
        let lastStmt = NoBindS $ AppE thePure r
        pure $ DoE Nothing (reverse (lastStmt:stmts))

generateGetParams :: [(T.Text, Field)] -> Q ([Exp], [Stmt])
generateGetParams = go (mempty, [], [])
  where
    go :: (M.Map T.Text Name, [Exp], [Stmt]) -> [(T.Text, Field)] -> Q ([Exp], [Stmt])
    go (_, a, b) [] = pure (reverse a, b)
    go (s, names, stmts) ((name, Field _):rest) = do
        nn <- newName (T.unpack name)
        let newStmt = BindS (VarP nn) (VarE (mkName "Data.Binary.get"))
        go (M.insert name nn s, VarE nn:names, newStmt:stmts) rest
    go (s, names, stmts) ((name, Conditional v midx _):rest) = do
        nn <- newName (T.unpack name)
        l <- tr_ midx
        let newStmt = BindS (VarP nn) (AppE (AppE (VarE (mkName "Data.Binary.tlHandleOpt")) (VarE (s M.! v))) l)
        go (M.insert name nn s, VarE nn:names, newStmt:stmts) rest
      where
        tr_ Nothing = [| Nothing |]
        tr_ (Just x) = do
            let r = pure $ LitE $ IntegerL (fromIntegral x)
            [| Just $r |]
