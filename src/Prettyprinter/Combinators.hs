----------------------------------------------------------------------------
-- |
-- Module      :  Prettyprinter.Combinators
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD2 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  21 March 2018
----------------------------------------------------------------------------

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Prettyprinter.Combinators
  ( Pretty(..)
  , PP.Doc
  , putDocLn
  , displayDoc
  , displayDocString
  , docFromString
  , docFromText
  , PP.viaShow

  , MapEntry(..)
  , (-->)
  , ppMapEntryWith
  , ppMapEntryWithSep
  , ppList
  , (##)
  , ppDictHeader
  , ppDictAssocList

  , ppTuple
  , ppTupleWith
  , ppListWith
  , ppFoldableHeader
  , ppFoldableHeaderWith
  , ppNE
  , ppNEWith
  , ppMap
  , ppMapWith
  , ppSet
  , ppSetWith
  , ppBimap
  , ppBimapWith
  , ppIntSet
  , ppIntSetWith
  , ppIntMap
  , ppIntMapWith
  , ppHashSet
  , ppHashSetWith
  , ppHashMap
  , ppHashMapWith
  , ppVector
  , ppVectorWith
  , ppDList
  , ppDListWith
  , ppListWithDelim
  , ppAssocList
  , ppAssocListWith
  , ppAssocListWithSep
  , ppByteString
  , ppByteStringLazy
  , ppShortByteString
  , ppTrace
  , ppCallStack
  , ppCallStackGHC
  ) where

import Data.Bimap (Bimap)
import qualified Data.Bimap as BM
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as CL8
import qualified Data.ByteString.Short as ShortBS
import Data.DList (DList)
import qualified Data.DList as DL
import Data.Foldable
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Debug.Trace
import GHC.Stack (CallStack, SrcLoc(..), getCallStack, prettySrcLoc)

#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup ((<>))
#endif

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Prettyprinter (Pretty(..), Doc, (<+>))
import qualified Prettyprinter as PP
import Prettyprinter.Combinators.Basic
import Prettyprinter.MetaDoc
import qualified Prettyprinter.Render.Text as PP.Render

putDocLn :: Doc ann -> IO ()
putDocLn x = do
  PP.Render.putDoc x
  putStrLn ""

displayDoc :: Doc ann -> TL.Text
displayDoc = PP.Render.renderLazy . PP.layoutPretty PP.defaultLayoutOptions

displayDocString :: Doc ann -> String
displayDocString = TL.unpack . displayDoc

docFromString :: String -> Doc ann
docFromString = pretty . TL.pack

docFromText :: T.Text -> Doc ann
docFromText = pretty . TL.fromStrict


infixr 0 :->

data MapEntry k v = k :-> v
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

ppMapEntryWith
  :: (k -> Doc ann) -> (v -> Doc ann) -> MapEntry k v -> Doc ann
ppMapEntryWith = ppMapEntryWithSep "->"

ppMapEntryWithSep
  :: Doc ann -> (k -> Doc ann) -> (v -> Doc ann) -> MapEntry k v -> Doc ann
ppMapEntryWithSep sep f g (x :-> y) =
  PP.group $ f x <+> PP.nest 4 (sep <> PP.line <> PP.align (g y))

instance (Pretty k, Pretty v) => Pretty (MapEntry k v) where
  pretty = ppMapEntryWith pretty pretty

infixr 0 -->

-- | Helper to make constructing 'MapEntry' pairs easier by calling
-- pretty on value.
(-->) :: Pretty v => T.Text -> v -> MapEntry T.Text (Doc ann)
(-->) k v = k :-> pretty v

ppDictHeader :: Doc ann -> [MapEntry T.Text (Doc ann)] -> Doc ann
ppDictHeader header entries =
  header ## ppDictAssocList entries

ppDictAssocList :: [MapEntry T.Text (Doc ann)] -> Doc ann
ppDictAssocList entries = ppListWithDelim PP.lbrace PP.rbrace entries'
  where
    entries' = flip map entries $ \entry ->
      ppMapEntryWith (PP.fillBreak maxWidth . pretty) id entry `PP.flatAlt`
      ppMapEntryWith pretty id entry
    maxWidth = maximum $ map (\(k :-> _) -> T.length k) entries

ppList :: Pretty a => [a] -> Doc ann
ppList = ppListWith pretty

ppListWith :: (a -> Doc ann) -> [a] -> Doc ann
ppListWith f = ppListWithDelim PP.lbracket PP.rbracket . map f

ppTuple :: Pretty a => [a] -> Doc ann
ppTuple = ppTupleWith pretty

ppTupleWith :: (a -> Doc ann) -> [a] -> Doc ann
ppTupleWith f = ppListWithDelim PP.lparen PP.rparen . map f

ppFoldableHeader :: (Pretty a, Foldable f) => Doc ann -> f a -> Doc ann
ppFoldableHeader = ppFoldableHeaderWith pretty

ppFoldableHeaderWith
  :: Foldable f
  => (a -> Doc ann) -> Doc ann -> f a -> Doc ann
ppFoldableHeaderWith f header entries =
  PP.nest 2 $
  header <> PP.line <> PP.vsep (map (("-" PP.<+>) . PP.align . f) $ toList entries)

ppNE :: Pretty a => NonEmpty a -> Doc ann
ppNE = ppNEWith pretty

ppNEWith :: (a -> Doc ann) -> NonEmpty a -> Doc ann
ppNEWith f = ppListWithDelim PP.lbracket PP.rbracket . map f . toList

ppMap :: (Pretty a, Pretty b) => Map a b -> Doc ann
ppMap = ppMapWith pretty pretty

ppMapWith :: (k -> Doc ann) -> (v -> Doc ann) -> Map k v -> Doc ann
ppMapWith f g = ppAssocListWith f g . M.toList

ppSet :: Pretty a => Set a -> Doc ann
ppSet = ppSetWith pretty

ppSetWith :: (a -> Doc ann) -> Set a -> Doc ann
ppSetWith f = ppListWithDelim PP.lbrace PP.rbrace . map f . toList

ppBimap :: (Pretty k, Pretty v) => Bimap k v -> Doc ann
ppBimap = ppBimapWith pretty pretty

ppBimapWith :: (k -> Doc ann) -> (v -> Doc ann) -> Bimap k v -> Doc ann
ppBimapWith f g = ppAssocListWithSep "<->" f g . BM.toList

ppIntSet :: IntSet -> Doc ann
ppIntSet = ppIntSetWith pretty

ppIntSetWith :: (Int -> Doc ann) -> IntSet -> Doc ann
ppIntSetWith f = ppListWithDelim PP.lbrace PP.rbrace . map f . IS.toList

ppIntMap :: Pretty a => IntMap a -> Doc ann
ppIntMap = ppIntMapWith pretty pretty

ppIntMapWith :: (Int -> Doc ann) -> (a -> Doc ann) -> IntMap a -> Doc ann
ppIntMapWith f g = ppAssocListWith f g . IM.toList

ppHashSet :: Pretty a => HashSet a -> Doc ann
ppHashSet = ppHashSetWith pretty

ppHashSetWith :: (a -> Doc ann) -> HashSet a -> Doc ann
ppHashSetWith f = ppListWithDelim PP.lbrace PP.rbrace . map f . HS.toList

ppHashMap :: (Pretty k, Pretty v) => HashMap k v -> Doc ann
ppHashMap = ppHashMapWith pretty pretty

ppHashMapWith :: (k -> Doc ann) -> (v -> Doc ann) -> HashMap k v -> Doc ann
ppHashMapWith f g = ppAssocListWith f g . HM.toList

ppVector :: Pretty a => Vector a -> Doc ann
ppVector = ppVectorWith pretty

ppVectorWith :: (a -> Doc ann) -> Vector a -> Doc ann
ppVectorWith f = ppListWith f . V.toList

ppDList :: Pretty a => DList a -> Doc ann
ppDList = ppDListWith pretty

ppDListWith :: (a -> Doc ann) -> DList a -> Doc ann
ppDListWith f = ppListWith f . DL.toList

ppListWithDelim
  :: forall f ann. (Functor f, Foldable f)
  => Doc ann
  -> Doc ann
  -> f (Doc ann)
  -> Doc ann
ppListWithDelim = ppListWithDelimSep separator
  where
    separator = ","

ppAssocList :: (Pretty k, Pretty v) => [(k, v)] -> Doc ann
ppAssocList =
  ppAssocListWith pretty pretty

ppAssocListWith :: (k -> Doc ann) -> (v -> Doc ann) -> [(k, v)] -> Doc ann
ppAssocListWith =
  ppAssocListWithSep "->"

ppAssocListWithSep
  :: Doc ann
  -> (k -> Doc ann)
  -> (v -> Doc ann)
  -> [(k, v)]
  -> Doc ann
ppAssocListWithSep sep f g =
  ppListWithDelim PP.lbrace PP.rbrace . map (ppMapEntryWithSep sep f g . uncurry (:->))

ppByteString :: C8.ByteString -> Doc ann
ppByteString = mdPayload . strictByteStringMetaDoc

ppByteStringLazy :: CL8.ByteString -> Doc ann
ppByteStringLazy = mdPayload . lazyByteStringMetaDoc

ppShortByteString :: ShortBS.ShortByteString -> Doc ann
ppShortByteString = mdPayload . shortByteStringMetaDoc

ppTrace :: Doc ann -> a -> a
ppTrace msg = trace (displayDocString msg)

ppCallStack :: CallStack -> Doc ann
ppCallStack =
  PP.vcat .
  map (\(name, loc) -> PP.hcat
        [ docFromString (srcLocModule loc)
        , "."
        , docFromString name
        , ":"
        , pretty (srcLocStartLine loc)
        , ":"
        , pretty (srcLocStartCol loc)
        ]
        ) .
  getCallStack

-- | Pretty-print a CallStack just as GHC would.
ppCallStackGHC :: CallStack -> Doc ann
ppCallStackGHC =
  PP.vcat .
  map (\(name, loc) ->
        docFromString name <> ", called at" <+> docFromString (prettySrcLoc loc)) .
  getCallStack
