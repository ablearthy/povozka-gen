{-# LANGUAGE OverloadedStrings #-}

module Povozka.Gen.Name where

import Data.ByteString.Lazy qualified as BS
import Data.Char (toLower, toUpper)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TE

fromByteString :: BS.ByteString -> T.Text
fromByteString = TL.toStrict . TE.decodeUtf8

toVarName :: T.Text -> T.Text
toVarName t = '_' `T.cons` coCapitalize t

replaceDots :: T.Text -> T.Text
replaceDots = T.replace "." "'"

capitalize :: T.Text -> T.Text
capitalize t = if T.null t then t else toUpper (T.head t) `T.cons` T.tail t

coCapitalize :: T.Text -> T.Text
coCapitalize t = if T.null t then t else toLower (T.head t) `T.cons` T.tail t

toTypeName :: T.Text -> T.Text
toTypeName = capitalize . replaceDots

toTypeName' :: T.Text -> T.Text
toTypeName' = (`T.snoc` '\'') . toTypeName
