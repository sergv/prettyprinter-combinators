----------------------------------------------------------------------------
-- |
-- Module      :  PrettyprinterCombinatorTests
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  26 March 2018
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE GADTs           #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}

{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -ddump-splices #-}

module PrettyprinterCombinatorTests (main) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vector

import Data.Text.Prettyprint.Doc
-- import Data.Text.Prettyprint.Doc.TH

data Test = Test
  { testSet         :: Maybe (Set Int)
  , testMap         :: Map String (Set Double)
  , testIntSet      :: IntSet
  , testIntMap      :: IntMap String
  , testInt         :: Int
  , testComplexMap  :: Map (Maybe (Set Int)) (IntMap (Set String))
  , testComplexMap2 :: Map (Maybe (HashSet Int)) (HashMap (NonEmpty Int) (Vector String))
  }

data Test2 =
    Foo Int Int Double (Maybe Test2)
  | Bar (String, Int, Int) (Map String Int) (HashMap String Int) (Maybe Test2) (NonEmpty Int)

data List a = Nil | Cons a (List a)

type Foo = Int
type a :+: b = Either a b

-- Make types available in type environment of following splices.
$(return [])


ppFoo :: Foo -> Doc ann
ppFoo = $(testPP [t| forall b. (Ord b, b ~ Int) => List (Maybe b) |])


-- ppTest :: Test -> Doc ann
-- ppTest = $(derivePP ''Test)

-- ppTest2 :: Test2 -> Doc ann
-- ppTest2 = $(derivePP [t| Test2 |])

-- ppFoo :: Foo -> Doc ann
-- ppFoo = $(derivePP [t| Foo |])

-- ppList :: List Int -> Doc ann
-- ppList = $(derivePP [t| List Int |])
--
-- ppTrickyList :: List (Maybe Int) -> Doc ann
-- ppTrickyList = $(derivePP [t| forall b. (Ord b, b ~ Int) => List (Maybe b) |])
--
-- ppVileHideousCrazyMadList :: List (Maybe (List Int)) -> Doc ann
-- ppVileHideousCrazyMadList = $(derivePP [t| forall b c d. (b ~ Maybe c, c ~ List d, d ~ Int) => List b |])
--
-- ppWat :: List (Double, Maybe [Int], [Int], Int) -> Doc ann
-- ppWat = $(derivePP [t| forall a b c d. (Ord a, b ~ Maybe c, c ~ [d], d ~ Int, a ~ Double) => List (a, b, c, d) |])
--
-- ppWat2 :: List (Double, Int, Int, Int, Int, [Int]) -> Doc ann
-- ppWat2 =
--   $(derivePP
--       [t| forall a b c d e f.
--              (Ord a, b ~ c, c ~ d, d ~ e, e ~ b, f ~ [d], d ~ Int, a ~ Double, d ~ Int)
--           => List (a, b, c, d, e, f) |])
--
-- ppWat4 :: List Int -> Doc ann
-- ppWat4 = $(derivePP [t| forall a b. (b ~ Int, a ~ List) => a b |])

-- wat :: forall a b c d. (Ord a, b ~ Maybe c, c ~ [d]) => List (a, b, c, d)
-- wat = undefined
--
-- wat2
--   :: forall a b c d e f. (Ord a, b ~ c, c ~ d, d ~ e, e ~ b, f ~ [d])
--   => a -> b -> c -> d -> e -> f -> List (a, b, c, d, e, f)
-- wat2 a b c d e f = Cons (a, b, c, d, e, f) Nil
--
-- wat3 :: List (Int, Int, Int, Int, Int, [Int])
-- wat3 = wat2 1 2 3 4 5 [6]
--
-- wat4 :: (b ~ Int, a ~ List) => a b
-- wat4 = undefined

main :: IO ()
main = do
  putStrLn "Compilation worked!"
  pure ()
