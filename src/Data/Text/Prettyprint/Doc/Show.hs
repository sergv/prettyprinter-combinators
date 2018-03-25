----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Prettyprint.Doc.Show
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  25 March 2018
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Text.Prettyprint.Doc.Show (ppShow) where

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc as PP
import Data.Text.Prettyprint.Doc.Combinators
import Data.Text.Prettyprint.Doc.MetaDoc
import Text.Show.Pretty (parseValue, Value(..))

ppShow :: Show a => a -> Doc ann
ppShow x =
  case parseValue y of
    Nothing -> pretty y
    Just y' -> mdPayload $ ppValue y'
  where
    y :: String
    y = show x

ppValue :: Value -> MetaDoc ann
ppValue = \case
  Con name args   ->
    constructorAppMetaDoc (atomicMetaDoc (pretty name)) (map ppValue args)
  InfixCons v xs  ->
    compositeMetaDoc $
    hsep (mdPayload (ppValue v) : concatMap (\(con, v') -> [pretty con, mdPayload (ppValue v')]) xs)
  Rec name fields ->
    compositeMetaDoc $
    ppDictHeader (pretty name) (map (\(field, v) -> T.pack field :-> mdPayload (ppValue v)) fields)
  Tuple xs        ->
    atomicMetaDoc $
    ppListWithDelim PP.lparen PP.rparen $ map (mdPayload . ppValue) xs
  List xs         ->
    atomicMetaDoc $
    ppListWith (mdPayload . ppValue) xs
  Neg x           -> atomicMetaDoc $ "-" <> mdPayload (ppValue x)
  Ratio x y       -> compositeMetaDoc $ mdPayload (ppValue x) <+> "%" <+> mdPayload (ppValue y)
  Integer x       -> stringMetaDoc x
  Float x         -> stringMetaDoc x
  Char x          -> stringMetaDoc x
  String x        -> stringMetaDoc x
