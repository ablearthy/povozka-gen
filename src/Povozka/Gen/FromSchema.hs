{-# LANGUAGE OverloadedStrings #-}

module Povozka.Gen.FromSchema (obtainContext, term2intermediateType, pprintIntermediateType, extractTypes) where

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
obtainContext = M.foldlWithKey' go emptyContext . M.union predefinedTypes . extracted_types
  where
    emptyContext = Context mempty mempty mempty

    extracted_types :: [P.Decl meta] -> M.Map T.Text T.Text
    extracted_types =
      foldl'
        ( \m d@(P.Decl c _ _ _) ->
            M.insert (fullCombinatorId2text c) (getRawTypeName d) m
        )
        M.empty

    go :: Context -> T.Text -> T.Text -> Context
    go (Context s m t2c) c t =
      Context
        (S.insert t s)
        (M.insert c t m)
        (M.insertWith S.union t (S.singleton c) t2c)

    predefinedTypes = M.fromList [("int", "Int"), ("long", "Long"), ("bytes", "Bytes"), ("vector", "Vector"), ("string", "String")]

identifier2text :: P.Identifier meta -> T.Text
identifier2text (P.Identifier _ (Just ns) name) = fromByteString ns <> "." <> fromByteString name
identifier2text (P.Identifier _ _ name) = fromByteString name

fullCombinatorId2text :: P.FullCombinatorId meta -> T.Text
fullCombinatorId2text (P.FullCombinatorId _meta ns name _) = identifier2text (P.Identifier _meta ns name)

extractTypes :: P.Schema meta -> TypeMap
extractTypes schema = foldl' go mempty constructors
  where
    constructors = P.constrDecls schema

    go :: TypeMap -> P.Decl meta -> TypeMap
    go m d@(P.Decl c _ _ _) =
      M.insertWith
        M.union
        (getRawTypeName d)
        (M.singleton (fullCombinatorId2text c) (buildCombinator d))
        m

    buildCombinator :: P.Decl meta -> Combinator
    buildCombinator (P.Decl fc@(P.FullCombinatorId _ _ _ constrId) _ args (P.ResultType rtHead (P.Expr rtTail))) =
      Combinator
        { constr = SimpleType $ toTypeName' $ fullCombinatorId2text fc
        , constrId = constrId
        , typeName = term2intermediateType ctx (P.TExpr (P.Expr (P.TVar rtHead : rtTail)))
        , fields = map arg_to_field args
        }

    arg_to_field :: P.Arg meta -> (VarName, Field)
    arg_to_field (P.ArrayArg {}) = error "array args are not supported"
    arg_to_field (P.SimpleArg Nothing _ _ _) = error "anonymous fields are not supported"
    arg_to_field (P.SimpleArg (Just i) Nothing _ t) = (toVarName (identifier2text i), Field (term2intermediateType ctx t))
    arg_to_field (P.SimpleArg (Just i) (Just (n, idx)) _ t) = (toVarName (identifier2text i), Conditional (toVarName (identifier2text n)) idx (term2intermediateType ctx t))

    ctx = obtainContext constructors

getRawTypeName :: P.Decl meta -> TypeName
getRawTypeName (P.Decl _ _ _ (P.ResultType identifier _)) = identifier2text identifier

data Boxity = Boxed | Bare deriving (Eq)

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