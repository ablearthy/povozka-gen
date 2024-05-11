{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (forM_)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Povozka.Gen.FromSchema (extractFunctions, extractTypes)
import Povozka.Gen.TH (generateBinaryInstanceForConstructor, generateBinaryInstanceForTypeFamily', generateFunction, generateTypeFamily, pprint, runQ)
import Povozka.Parser (parseTL, runAlex)

main :: IO ()
main = do
  contents <- BS.readFile "./test.tl"
  case runAlex contents parseTL of
    Left e -> print e
    Right schema -> do
      let m = extractTypes schema
      let f = extractFunctions schema
      forM_ (M.toList m) $ \(t, constrs) -> do
        forM_ (M.toList constrs) $ \(rawConstrName, comb) -> do
          ex <- runQ (generateBinaryInstanceForConstructor False comb)
          putStrLn $ pprint ex
        let expr = generateTypeFamily (M.elems constrs)
        ex <- runQ (generateBinaryInstanceForTypeFamily' (M.elems constrs))
        putStrLn $ pprint expr
        putStrLn $ pprint ex
      forM_ f $ \r -> do
        ex <- runQ (generateFunction r)
        putStrLn $ pprint ex

-- let constructors = constrDecls schema
-- let ctx = obtainContext constructors
-- forM_ constructors $ \(Decl _ _ args _) -> do
--   let terms = map (\(SimpleArg _ _ _ t) -> t) args
--   let irs = map (term2intermediateType ctx) terms
--   forM_ irs $ putStrLn <$> pprintIntermediateType
-- print ctx
