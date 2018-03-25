----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Prettyprint.Doc.Generics
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  24 March 2018
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Text.Prettyprint.Doc.Generics (ppGeneric) where

import Data.Bimap (Bimap)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as CL8
import qualified Data.ByteString.Short as ShortBS
import Data.DList (DList)
import qualified Data.DList as DList
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Int
import Data.IntMap (IntMap)
import qualified Data.IntSet as IntSet
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Proxy
import Data.Semigroup
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Vector (Vector)
import Data.Void
import Data.Word
import GHC.Generics
import GHC.Stack (CallStack)
import GHC.TypeLits

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Combinators
import Data.Text.Prettyprint.Doc.MetaDoc

-- $setup
-- >>> :set -XDeriveGeneric
-- >>> import Data.List.NonEmpty (NonEmpty(..))
-- >>> import qualified Data.List.NonEmpty as NonEmpty
-- >>> import Data.HashMap.Strict (HashMap)
-- >>> import qualified Data.HashMap.Strict as HashMap
-- >>> import Data.HashSet (HashSet)
-- >>> import qualified Data.HashSet as HashSet
-- >>> import Data.IntMap (IntMap)
-- >>> import qualified Data.IntMap as IntMap
-- >>> import Data.IntSet (IntSet)
-- >>> import qualified Data.IntSet as IntSet
-- >>> import Data.Map.Strict (Map)
-- >>> import qualified Data.Map.Strict as Map
-- >>> import Data.Set (Set)
-- >>> import qualified Data.Set as Set
-- >>> import Data.Vector (Vector)
-- >>> import qualified Data.Vector as Vector
-- >>> import GHC.Generics (Generic)
--
-- >>> :{
-- data Test = Test
--   { testSet         :: Maybe (Set Int)
--   , testMap         :: Map String (Set Double)
--   , testIntSet      :: IntSet
--   , testIntMap      :: IntMap String
--   , testInt         :: Int
--   , testComplexMap  :: Map (Maybe (Set Int)) (IntMap (Set String))
--   , testComplexMap2 :: Map (Maybe (HashSet Int)) (HashMap (NonEmpty Int) (Vector String))
--   } deriving (Generic)
-- :}

-- | Prettyprint using 'Generic.Data' instance.
--
-- >>> :{
-- test = Test
--   { testSet         = Just $ Set.fromList [1..3]
--   , testMap         =
--       Map.fromList [("foo", Set.fromList [1.5]), ("foo", Set.fromList [2.5, 3, 4])]
--   , testIntSet      = IntSet.fromList [1, 2, 4, 5, 7]
--   , testIntMap      = IntMap.fromList $ zip [1..] ["3", "2foo", "11"]
--   , testInt         = 42
--   , testComplexMap  = Map.fromList
--       [ ( Nothing
--         , IntMap.fromList $ zip [0..] $ map Set.fromList
--             [ ["foo", "bar"]
--             , ["baz"]
--             , ["quux", "frob"]
--             ]
--         )
--       , ( Just (Set.fromList [1])
--         , IntMap.fromList $ zip [0..] $ map Set.fromList
--             [ ["quux"]
--             , ["fizz", "buzz"]
--             ]
--         )
--       , ( Just (Set.fromList [3, 4])
--         , IntMap.fromList $ zip [0..] $ map Set.fromList
--             [ ["quux", "5"]
--             , []
--             , ["fizz", "buzz"]
--             ]
--         )
--       ]
--   , testComplexMap2 =
--       Map.singleton
--         (Just (HashSet.fromList [1..5]))
--         (HashMap.fromList
--            [ (NonEmpty.fromList [1, 2],  Vector.fromList ["foo", "bar", "baz"])
--            , (NonEmpty.fromList [3],     Vector.fromList ["quux"])
--            , (NonEmpty.fromList [4..10], Vector.fromList ["must", "put", "something", "in", "here"])
--            ])
--   }
-- :}
--
-- >>> ppGeneric test
-- Test
--   { testSet         -> Just ({1, 2, 3})
--   , testMap         -> {foo -> {2.5, 3.0, 4.0}}
--   , testIntSet      -> {1, 2, 4, 5, 7}
--   , testIntMap      -> {1 -> 3, 2 -> 2foo, 3 -> 11}
--   , testInt         -> 42
--   , testComplexMap  ->
--       { Nothing -> {0 -> {bar, foo}, 1 -> {baz}, 2 -> {frob, quux}}
--       , Just ({1}) -> {0 -> {quux}, 1 -> {buzz, fizz}}
--       , Just ({3, 4}) -> {0 -> {5, quux}, 1 -> {}, 2 -> {buzz, fizz}}
--       }
--   , testComplexMap2 ->
--       { Just ({1, 2, 3, 4, 5}) ->
--           { [4, 5, 6, 7, 8, 9, 10] -> [must, put, something, in, here]
--           , [3] -> [quux]
--           , [1, 2] -> [foo, bar, baz]
--           } }
--   }
ppGeneric :: (Generic a, GPretty (Rep a)) => a -> Doc ann
ppGeneric = mdPayload . gpretty . from

class GPretty (a :: * -> *) where
  gpretty :: a ix -> MetaDoc ann

instance GPretty V1 where
  gpretty _ = error "gpretty for V1"

instance GPretty U1 where
  gpretty U1 = mempty

instance (GPretty f, GPretty g) => GPretty (f :+: g) where
  gpretty = \case
    L1 x -> gpretty x
    R1 y -> gpretty y

-- Use 'PPGenericDeriving' to give it a chance to fire before standard 'Pretty'.
instance PPGenericOverride c => GPretty (K1 i c) where
  gpretty = ppGenericOverride . unK1


-- | A class to override 'Pretty' when calling 'ppGeneric' without introducing
-- orphans for standard types.
class PPGenericOverride a where
  ppGenericOverride :: a -> MetaDoc ann

ppGenericOverrideDoc :: PPGenericOverride a => a -> Doc ann
ppGenericOverrideDoc = mdPayload . ppGenericOverride

newtype PPGenericOverrideToPretty a = PPGenericOverrideToPretty { unPPGenericOverrideToPretty :: a }

instance PPGenericOverride a => Pretty (PPGenericOverrideToPretty a) where
  pretty = mdPayload . ppGenericOverride . unPPGenericOverrideToPretty


-- | Fall back to standard 'Pretty' instance when no override is available.
instance Pretty a => PPGenericOverride a where
  ppGenericOverride = compositeMetaDoc . pretty



instance {-# OVERLAPS #-} PPGenericOverride Int where
  ppGenericOverride = metaDocInt

instance {-# OVERLAPS #-} PPGenericOverride Float where
  ppGenericOverride = metaDocFloat

instance {-# OVERLAPS #-} PPGenericOverride Double where
  ppGenericOverride = metaDocDouble

instance {-# OVERLAPS #-} PPGenericOverride Integer where
  ppGenericOverride = metaDocInteger

instance {-# OVERLAPS #-} PPGenericOverride Word where
  ppGenericOverride = metaDocWord

instance {-# OVERLAPS #-} PPGenericOverride Word8 where
  ppGenericOverride = metaDocWord8

instance {-# OVERLAPS #-} PPGenericOverride Word16 where
  ppGenericOverride = metaDocWord16

instance {-# OVERLAPS #-} PPGenericOverride Word32 where
  ppGenericOverride = metaDocWord32

instance {-# OVERLAPS #-} PPGenericOverride Word64 where
  ppGenericOverride = metaDocWord64

instance {-# OVERLAPS #-} PPGenericOverride Int8 where
  ppGenericOverride = metaDocInt8

instance {-# OVERLAPS #-} PPGenericOverride Int16 where
  ppGenericOverride = metaDocInt16

instance {-# OVERLAPS #-} PPGenericOverride Int32 where
  ppGenericOverride = metaDocInt32

instance {-# OVERLAPS #-} PPGenericOverride Int64 where
  ppGenericOverride = metaDocInt64

instance {-# OVERLAPS #-} PPGenericOverride () where
  ppGenericOverride = metaDocUnit

instance {-# OVERLAPS #-} PPGenericOverride Bool where
  ppGenericOverride = metaDocBool

instance {-# OVERLAPS #-} PPGenericOverride Char where
  ppGenericOverride = metaDocChar

instance {-# OVERLAPS #-} PPGenericOverride CallStack where
  ppGenericOverride =
    compositeMetaDoc . ppCallStack


instance {-# OVERLAPS #-} PPGenericOverride (Doc Void) where
  ppGenericOverride =
    compositeMetaDoc . fmap absurd

instance {-# OVERLAPS #-} PPGenericOverride String where
  ppGenericOverride = stringMetaDoc

instance {-# OVERLAPS #-} PPGenericOverride T.Text where
  ppGenericOverride = strictTextMetaDoc

instance {-# OVERLAPS #-} PPGenericOverride TL.Text where
  ppGenericOverride = lazyTextMetaDoc

instance {-# OVERLAPS #-} PPGenericOverride C8.ByteString where
  ppGenericOverride = strictByteStringMetaDoc

instance {-# OVERLAPS #-} PPGenericOverride CL8.ByteString where
  ppGenericOverride = lazyByteStringMetaDoc

instance {-# OVERLAPS #-} PPGenericOverride ShortBS.ShortByteString where
  ppGenericOverride = shortByteStringMetaDoc


instance {-# OVERLAPS #-} PPGenericOverride v => PPGenericOverride (Maybe v) where
  ppGenericOverride =
    gpretty . from . fmap PPGenericOverrideToPretty

instance {-# OVERLAPS #-} PPGenericOverride v => PPGenericOverride [v] where
  ppGenericOverride =
    atomicMetaDoc . ppListWith ppGenericOverrideDoc

instance {-# OVERLAPS #-} (PPGenericOverride k, PPGenericOverride v) => PPGenericOverride [(k, v)] where
  ppGenericOverride =
    atomicMetaDoc . ppAssocListWith ppGenericOverrideDoc ppGenericOverrideDoc

instance {-# OVERLAPS #-} PPGenericOverride k => PPGenericOverride (NonEmpty k) where
  ppGenericOverride =
    atomicMetaDoc . ppNEWith ppGenericOverrideDoc

instance {-# OVERLAPS #-} PPGenericOverride v => PPGenericOverride (Vector v) where
  ppGenericOverride =
    atomicMetaDoc . ppVectorWith ppGenericOverrideDoc


instance {-# OVERLAPS #-} (PPGenericOverride k, PPGenericOverride v) => PPGenericOverride (Map k v) where
  ppGenericOverride =
    atomicMetaDoc . ppMapWith ppGenericOverrideDoc ppGenericOverrideDoc

instance {-# OVERLAPS #-} PPGenericOverride v => PPGenericOverride (Set v) where
  ppGenericOverride =
    atomicMetaDoc . ppSetWith ppGenericOverrideDoc

instance {-# OVERLAPS #-} (PPGenericOverride k, PPGenericOverride v) => PPGenericOverride (Bimap k v) where
  ppGenericOverride =
    atomicMetaDoc . ppBimapWith ppGenericOverrideDoc ppGenericOverrideDoc

instance {-# OVERLAPS #-} PPGenericOverride IntSet.IntSet where
  ppGenericOverride =
    atomicMetaDoc . ppIntSetWith ppGenericOverrideDoc

instance {-# OVERLAPS #-} PPGenericOverride v => PPGenericOverride (IntMap v) where
  ppGenericOverride =
    atomicMetaDoc . ppIntMapWith ppGenericOverrideDoc ppGenericOverrideDoc

instance {-# OVERLAPS #-} PPGenericOverride v => PPGenericOverride (HashSet v) where
  ppGenericOverride =
    atomicMetaDoc . ppHashSetWith ppGenericOverrideDoc

instance {-# OVERLAPS #-} (PPGenericOverride k, PPGenericOverride v) => PPGenericOverride (HashMap k v) where
  ppGenericOverride =
    atomicMetaDoc . ppHashMapWith ppGenericOverrideDoc ppGenericOverrideDoc


instance (GPretty f, GPretty g) => GPretty (f :*: g) where
  gpretty (x :*: y) =
    compositeMetaDoc $ mdPayload x' <+> mdPayload y'
    where
      x' = gpretty x
      y' = gpretty y

instance GPretty f => GPretty (M1 D ('MetaData a b c d) f) where
  gpretty = gpretty . unM1

instance GPretty f => GPretty (M1 S ('MetaSel 'Nothing b c d) f) where
  gpretty = gpretty . unM1

instance (KnownSymbol name, GPretty f, GFields f) => GPretty (M1 C ('MetaCons name _fixity 'False) f) where
  gpretty (M1 x) =
    constructorAppMetaDoc constructor args
    where
      constructor :: MetaDoc ann
      constructor = atomicMetaDoc $ pretty $ symbolVal $ Proxy @name
      args :: [MetaDoc ann]
      args = toList $ gfields x

class GFields a where
  gfields :: a ix -> DList (MetaDoc ann)

instance GFields U1 where
  gfields = const mempty

instance GPretty f => GFields (M1 S ('MetaSel a b c d) f) where
  gfields = DList.singleton . gpretty . unM1

instance (GFields f, GFields g) => GFields (f :*: g) where
  gfields (f :*: g) = gfields f <> gfields g


instance (KnownSymbol name, GCollectRecord f) => GPretty (M1 C ('MetaCons name _fixity 'True) f) where
  gpretty (M1 x) =
    compositeMetaDoc $
      ppDictHeader
        (pretty (symbolVal (Proxy @name)))
        (map (fmap mdPayload) (toList (gcollectRecord x)))

class GCollectRecord a where
  gcollectRecord :: a ix -> DList (MapEntry Text (MetaDoc ann))

instance (KnownSymbol name, GPretty a) => GCollectRecord (M1 S ('MetaSel ('Just name) su ss ds) a) where
  gcollectRecord (M1 x) =
    DList.singleton (T.pack (symbolVal (Proxy @name)) :-> gpretty x)

instance (GCollectRecord f, GCollectRecord g) => GCollectRecord (f :*: g) where
  gcollectRecord (f :*: g) = gcollectRecord f <> gcollectRecord g

instance GCollectRecord U1 where
  gcollectRecord = const mempty
