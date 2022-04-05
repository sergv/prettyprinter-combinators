----------------------------------------------------------------------------
-- |
-- Module      :  Prettyprinter.Data
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prettyprinter.Data
  ( ppData
  , ppDataSimple
  ) where

import Data.Data
import Data.Generics qualified
import Prettyprinter
import Prettyprinter qualified as PP
import Prettyprinter.Combinators
import Prettyprinter.MetaDoc

-- $setup
-- >>> :set -XDeriveDataTypeable
-- >>> import Data.List.NonEmpty (NonEmpty(..))
-- >>> import qualified Data.List.NonEmpty as NonEmpty
-- >>> import Data.HashMap.Strict (HashMap)
-- >>> import qualified Data.HashMap.Strict as HashMap
-- >>> import Data.Map.Strict (Map)
-- >>> import qualified Data.Map.Strict as Map
-- >>> import Data.Data
--
-- >>> :{
-- data Test =
--     Foo Int [Int] Double (Maybe Test)
--   | Bar (String, Int, Int) (Map String Int) (HashMap String Int) (Maybe Test) (NonEmpty Int)
--   deriving (Data)
-- :}

-- | Prettyprint using 'Data.Data' instance.
--
-- >>> :{
-- test =
--   Bar
--     ("foo", 10, 20)
--     (Map.fromList (zip ["foo", "bar", "baz"] [1..]))
--     (HashMap.fromList (zip ["foo", "bar", "baz", "quux", "fizz", "buzz", "frob", "wat"] [1..]))
--     (Just
--       (Foo
--          1
--          []
--          3.14159265358979323846264338327950288
--          (Just
--             (Foo
--                1
--                [2]
--                2.71828182
--                (Just (Bar ("x", 1, 2) mempty mempty Nothing (NonEmpty.fromList [42])))))))
--     (NonEmpty.fromList [1..42])
-- :}
--
-- >>> ppData test
-- Bar
--   (foo, 10, 20)
--   {bar -> 2, baz -> 3, foo -> 1}
--   { fizz -> 5
--   , baz -> 3
--   , quux -> 4
--   , foo -> 1
--   , buzz -> 6
--   , bar -> 2
--   , frob -> 7
--   , wat -> 8
--   }
--   Just Foo
--          1
--          {}
--          3.141592653589793
--          Just (Foo 1 [2] 2.71828182 (Just (Bar (x, 1, 2) {} {} Nothing [42])))
--   [ 1
--   , 2
--   , 3
--   , 4
--   , 5
--   , 6
--   , 7
--   , 8
--   , 9
--   , 10
--   , 11
--   , 12
--   , 13
--   , 14
--   , 15
--   , 16
--   , 17
--   , 18
--   , 19
--   , 20
--   , 21
--   , 22
--   , 23
--   , 24
--   , 25
--   , 26
--   , 27
--   , 28
--   , 29
--   , 30
--   , 31
--   , 32
--   , 33
--   , 34
--   , 35
--   , 36
--   , 37
--   , 38
--   , 39
--   , 40
--   , 41
--   , 42
--   ]
ppData :: Data a => a -> Doc ann
ppData = mdPayload . gpretty

ppDataSimple :: Data a => a -> Doc ann
ppDataSimple = pretty . Data.Generics.gshow

gpretty :: forall a ann. Data a => a -> MetaDoc ann
gpretty =
  render
    `Data.Generics.extQ` stringMetaDoc
    `Data.Generics.extQ` strictTextMetaDoc
    `Data.Generics.extQ` lazyTextMetaDoc
    `Data.Generics.extQ` metaDocInt
    `Data.Generics.extQ` metaDocFloat
    `Data.Generics.extQ` metaDocDouble
    `Data.Generics.extQ` metaDocInteger
    `Data.Generics.extQ` metaDocWord
    `Data.Generics.extQ` metaDocWord8
    `Data.Generics.extQ` metaDocWord16
    `Data.Generics.extQ` metaDocWord32
    `Data.Generics.extQ` metaDocWord64
    `Data.Generics.extQ` metaDocInt8
    `Data.Generics.extQ` metaDocInt16
    `Data.Generics.extQ` metaDocInt32
    `Data.Generics.extQ` metaDocInt64
    `Data.Generics.extQ` metaDocUnit
    `Data.Generics.extQ` metaDocBool
    `Data.Generics.extQ` metaDocChar
    -- Probably requires qualified constrtains...
    -- `Data.Generics.extQ`
    --   ((atomicMetaDoc . ppMapWith (mdPayload . gpretty) (mdPayload . gpretty)) ::
    --     forall k v. (Data k, Data v) => Map k v -> MetaDoc ann)
  where
    render :: Data b => b -> MetaDoc ann
    render t
      | constructorName == "fromList"
      , Just mapItems  <- gmapQi 0 (listElements (isPair gpretty)) t
      , Just mapItems' <- sequence mapItems
      = atomicMetaDoc
      $ ppAssocListWith mdPayload mdPayload mapItems'
      | Just mapItems  <- listElements (isPair gpretty) t
      , Just mapItems' <- sequence mapItems
      = atomicMetaDoc
      $ ppAssocListWith mdPayload mdPayload mapItems'
      | Just listItems <- listElements gpretty t
      = atomicMetaDoc
      $ ppListWith mdPayload listItems
      | isTuple
      = atomicMetaDoc
      $ ppListWithDelim PP.lparen PP.rparen
      $ map mdPayload fields
      | otherwise
      = constructorAppMetaDoc constructorDoc fields
      where
        constructorDoc :: MetaDoc ann
        constructorDoc = atomicMetaDoc $ pretty constructorName

        fields :: [MetaDoc ann]
        fields = gmapQ gpretty t

        constructorName :: String
        constructorName = showConstr $ toConstr t

        isTuple :: Bool
        isTuple = all (== ',') (filter (not . (`elem` ("()" :: String))) constructorName)

isPair :: Data a => (forall b. Data b => b -> c) -> a -> Maybe (c, c)
isPair f x
  | constructorName == "(,)" = Just (gmapQi 0 f x, gmapQi 1 f x)
  | otherwise                = Nothing
  where
    constructorName :: String
    constructorName = showConstr $ toConstr x

-- | Try to treat @a@ as a list and prettyprint its elements with @f@.
-- Returns Just on succes and Nothing if @a@ wasn't a list after all.
listElements :: forall a c. Data a => (forall b. Data b => b -> c) -> a -> Maybe [c]
listElements f = go
  where
    go :: Data d => d -> Maybe [c]
    go x
      | isNull    = Just []
      | isCons    = (:) (gmapQi 0 f x) <$> gmapQi 1 go x
      | otherwise = Nothing
      where
        constructorName :: String
        constructorName = showConstr $ toConstr x
        isCons = constructorName `elem` ["(:)", ":|"]
        isNull = constructorName == "[]"

