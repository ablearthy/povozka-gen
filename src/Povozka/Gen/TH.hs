{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Povozka.Gen.TH (generateConstructor, generateType, generateTypeFamily, generateBinaryInstanceForConstructor, generateBinaryInstanceForTypeFamily, generateBinaryInstanceForTypeFamily', pprint, runQ) where

import Language.Haskell.TH

import Data.Text qualified as T
import Povozka.Gen.Model

import Control.Monad (forM)
import Data.List (foldl')
import Data.Map.Strict qualified as M
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
  p <- bodyPut
  pure
    $ InstanceD
      Nothing
      []
      instanceName
      [ FunD
          (mkName "get")
          [ Clause [] (NormalB g) []
          ]
      , FunD (mkName "put") [p]
      ]
  where
    instanceName = AppT (ConT (mkName "Data.Binary.Binary")) (ConT combName)
    combName = text2name comb.constr

    bodyGet = do
      (names, stmts) <- generateGetParams comb.fields
      thePure <- runQ [|pure|]
      let r = foldl' AppE (ConE combName) names
      let lastStmt = NoBindS $ AppE thePure r
      pure $ DoE Nothing (reverse (lastStmt : stmts))
    bodyPut = do
      varName <- newName "to_be_encoded"
      let getter = referToField varName
      let generateSinglePutExpr fieldName = case fieldName `M.lookup` flags of
            Just children ->
              AppE
                (VarE (mkName "Data.Binary.encodeFlag"))
                (ListE (map (\(n, idx) -> TupE [Just (AppE (VarE (mkName "Data.Maybe.isJust")) (getter n)), Just (LitE (IntegerL (fromIntegral idx)))]) children))
            Nothing ->
              AppE
                (VarE (mkName "Data.Binary.put"))
                (getter fieldName)

      let statements = map (NoBindS . generateSinglePutExpr . fst) comb.fields
      pure $ Clause [VarP varName] (NormalB (DoE Nothing statements)) []
      where
        flags = collectFlags comb.fields

        referToField :: Name -> VarName -> Exp
        referToField varName fieldName = GetFieldE (VarE varName) (T.unpack fieldName)

generateGetParams :: [(T.Text, Field)] -> Q ([Exp], [Stmt])
generateGetParams = go (mempty, [], [])
  where
    go :: (M.Map T.Text Name, [Exp], [Stmt]) -> [(T.Text, Field)] -> Q ([Exp], [Stmt])
    go (_, a, b) [] = pure (reverse a, b)
    go (s, names, stmts) ((name, Field _) : rest) = do
      nn <- newName (T.unpack name)
      let newStmt = BindS (VarP nn) (VarE (mkName "Data.Binary.get"))
      go (M.insert name nn s, VarE nn : names, newStmt : stmts) rest
    go (s, names, stmts) ((name, Conditional v midx _) : rest) = do
      nn <- newName (T.unpack name)
      l <- tr_ midx
      let newStmt = BindS (VarP nn) (AppE (AppE (VarE (mkName "Data.Binary.tlHandleOpt")) (VarE (s M.! v))) l)
      go (M.insert name nn s, VarE nn : names, newStmt : stmts) rest
      where
        tr_ Nothing = [|Nothing|]
        tr_ (Just x) = do
          let r = pure $ LitE $ IntegerL (fromIntegral x)
          [|Just $r|]

generateBinaryInstanceForTypeFamily :: TypeName -> [Combinator] -> Q Dec
generateBinaryInstanceForTypeFamily typeName' combs = do
  g <- generateGet
  clauses <- forM combs generatePutClause
  pure
    $ InstanceD
      Nothing
      []
      instanceName
      [ FunD
          (mkName "get")
          [ Clause [] (NormalB g) []
          ]
      , FunD (mkName "put") clauses
      ]
  where
    instanceName = AppT (ConT (mkName "Data.Binary.Binary")) (ConT typeName)
    typeName = text2name typeName'

    strip_prime x = fromMaybe x (T.stripSuffix "'" x)

    generateGet = do
      tmpVar <- newName "tmp"
      let fstStmt = BindS (VarP tmpVar) (VarE (mkName "Data.Binary.Get.getWord32le"))

      let genMatch comb =
            Match
              (LitP (IntegerL (fromIntegral comb.constrId)))
              ( NormalB
                  ( VarE (mkName "fmap")
                      `AppE` VarE (text2name (strip_prime comb.constr))
                      `AppE` VarE (mkName "Data.Binary.get")
                  )
              )
              []
      let sndStmt = NoBindS $ CaseE (VarE tmpVar) (map genMatch combs)
      pure $ DoE Nothing [fstStmt, sndStmt]

    generatePutClause comb = do
      varName <- newName "tmp"
      let bodyStmt1 = AppE (VarE (mkName "Data.Binary.Put.putWord32le")) (LitE (IntegerL (fromIntegral comb.constrId)))
      let bodyStmt2 = AppE (VarE (mkName "Data.Binary.put")) (VarE varName)
      pure
        $ Clause
          [ConP (text2name (strip_prime comb.constr)) mempty [VarP varName]]
          (NormalB (DoE Nothing [NoBindS bodyStmt1, NoBindS bodyStmt2]))
          mempty

generateBinaryInstanceForTypeFamily' :: [Combinator] -> Q Dec
generateBinaryInstanceForTypeFamily' [] = error "given an empty family"
generateBinaryInstanceForTypeFamily' (x : xs) = generateBinaryInstanceForTypeFamily x.typeName (x : xs)

collectFlags :: [(VarName, Field)] -> M.Map VarName [(VarName, Int)]
collectFlags = foldl' go mempty
  where
    go :: M.Map VarName [(VarName, Int)] -> (VarName, Field) -> M.Map VarName [(VarName, Int)]
    go acc (fieldName, Conditional target (Just idx) _) = M.insertWith (++) target [(fieldName, idx)] acc
    go acc _ = acc