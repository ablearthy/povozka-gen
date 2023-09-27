{-# LANGUAGE OverloadedStrings #-}

module Povozka.Gen.FromSchema (obtainContext, term2intermediateType, pprintIntermediateType) where

import Povozka.Gen.Model
import Povozka.Gen.Name
import Povozka.Parser qualified as P

import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text qualified as T
import GHC.Stack (HasCallStack)

import Data.List (foldl')

type ConstrMap = M.Map TypeName Combinator
type TypeMap = M.Map TypeName ConstrMap

data Context = Context
  { typesSet :: !(S.Set T.Text)
  , constr2type :: !(M.Map T.Text T.Text)
  , type2constrs :: !(M.Map T.Text (S.Set T.Text))
  }
  deriving (Show)

isType :: Context -> T.Text -> Bool
isType ctx t = S.member t (typesSet ctx)

isType' :: Context -> P.Identifier meta -> Bool
isType' ctx r = isType ctx (identifier2text r)

obtainContext :: [P.Decl meta] -> Context
obtainContext = foldl' go (Context predefinedTypes predefinedConstr2Types predefinedType2Constrs)
  where
    go :: Context -> P.Decl meta -> Context
    go (Context s m t2c) d@(P.Decl c _ _ _) = Context (S.insert t s) (M.insert ctr t m) t2c'
      where
        t = getRawTypeName d
        ctr = fullCombinatorId2text c
        t2c' = M.insertWith S.union t (S.singleton ctr) t2c

    predefinedTypes = S.fromList ["Int", "Long", "Bytes", "Vector"]
    predefinedConstr2Types = M.fromList [("int", "Int"), ("long", "Long"), ("bytes", "Bytes"), ("vector", "Vector")]
    predefinedType2Constrs = M.fromList [("Int", S.singleton "int"), ("Long", S.singleton "long"), ("Bytes", S.singleton "bytes"), ("Vector", S.singleton "vector")]

identifier2text :: P.Identifier meta -> T.Text
identifier2text (P.Identifier _ (Just ns) name) = fromByteString ns <> "." <> fromByteString name
identifier2text (P.Identifier _ _ name) = fromByteString name

fullCombinatorId2text :: P.FullCombinatorId meta -> T.Text
fullCombinatorId2text (P.FullCombinatorId _meta ns name _) = identifier2text (P.Identifier _meta ns name)

extractTypes :: P.Schema meta -> TypeMap
extractTypes schema = undefined
  where
    constructors = P.constrDecls schema

getRawTypeName :: P.Decl meta -> TypeName
getRawTypeName (P.Decl _ _ _ (P.ResultType identifier _)) = identifier2text identifier

data Boxity = Boxed | Bare deriving Eq

data IntermediateType
  = SimpleType !TypeName
  | Ap IntermediateType IntermediateType
  deriving (Show)

pprintIntermediateType :: IntermediateType -> String
pprintIntermediateType (SimpleType x) = T.unpack x
pprintIntermediateType (Ap a b) = "(" <> pprintIntermediateType a <> " " <> pprintIntermediateType b <> ")"

term2intermediateType :: HasCallStack => Context -> P.Term meta -> IntermediateType
term2intermediateType ctx = go Boxed
  where
    go _ (P.TNumber _ _) = error "numbers are not supported"
    go boxity (P.TVar identifier)
      | isType' ctx identifier && (boxity == Bare) =
          let constrs = type2constrs ctx M.! identifier2text identifier
              constr =
                if S.size constrs == 1
                  then S.findMin constrs
                  else error "unable to choose constructor since there are multiple options"
          in  SimpleType $ toTypeName' constr
      | isType' ctx identifier = SimpleType $ toTypeName $ identifier2text identifier
      | otherwise = SimpleType $ toTypeName' $ identifier2text identifier
    go _ (P.TBare _ inner) = go Bare inner
    go _ (P.TExpr (P.Expr [])) = error "expr should not be empty"
    go boxity (P.TExpr (P.Expr (t : ts))) = case rest of
      Just rest' -> Ap first rest'
      Nothing -> first
      where
        first = go boxity t
        rest =
          let f term Nothing = Just $ go Boxed term
              f term (Just l) = Just $ Ap (go Boxed term) l
          in  foldr f Nothing ts

{- [Note] Naming conventions
   ~~~~~~~~~~~~~~~~~~~~~~~~~

   type <-> constrA, constrB, ...:
     data <constrA>' = <constrA>' ...
     data <constrB>' = <constrB>' ...

     data <type>
       = <constrA> <constrA>'
       | <constrB> <constrB>'
       | ...
  -}