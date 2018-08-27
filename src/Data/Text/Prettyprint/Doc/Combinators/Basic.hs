----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Prettyprint.Doc.Combinators.Basic
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

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Text.Prettyprint.Doc.Combinators.Basic
  ( (##)
  , ppListWithDelimSep
  ) where

import Data.Foldable
import Data.Semigroup as Semigroup
import Data.Text.Prettyprint.Doc as PP

(##) :: Doc ann -> Doc ann -> Doc ann
(##) x y = PP.nest 2 $ x Semigroup.<> PP.line <> y

{-# INLINABLE ppListWithDelimSep #-}
ppListWithDelimSep
  :: forall f ann. (Functor f, Foldable f)
  => Doc ann
  -> Doc ann
  -> Doc ann
  -> f (Doc ann)
  -> Doc ann
ppListWithDelimSep separator left right xs =
  case toList xs of
    []   -> left <> right
    [y]  -> PP.flatAlt (left <+> y <+> right) (left <> y <> right)
    y:ys ->
      PP.align $
        PP.group $
          PP.flatAlt
            (left PP.<+> y <> PP.line' <>
             PP.vcat fields <> PP.line <>
             right)
            (left PP.<> y <> PP.vcat fields <> right)
      where
        fields :: [Doc ann]
        fields = fmap (\x -> separator PP.<+> x) ys

