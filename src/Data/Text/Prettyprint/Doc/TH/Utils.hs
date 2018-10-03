----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Prettyprint.Doc.TH.Utils
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  serg.foo@gmail.com
-- Created     :  28 March 2018
-- Stability   :
-- Portability :
--
--
----------------------------------------------------------------------------

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Data.Text.Prettyprint.Doc.TH.Utils
  ( Fix(..)
  , cata
  , ana
  , TypeName(..)
  , TypeVarName(..)
  , ConstructorName(..)
  , THTypeF(..)
  , THType
  , THTranslationError(..)
  , fromTHType
  , toTHType
  , EqConstraints
  , getBndrVar
  ) where

import Control.Monad.Except
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Generics
import GHC.Generics (Generic)
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as Syn

#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup
#endif

import Data.Text.Prettyprint.Doc.Combinators

newtype Fix f = Fix { unFix :: f (Fix f) }

{-# INLINE cata #-}
cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = go
  where
    go = alg . fmap go . unFix

{-# INLINE ana #-}
ana :: Functor f => (a -> f a) -> a -> Fix f
ana alg = go
  where
    go = Fix . fmap go . alg

-- {-# INLINE anaM #-}
-- anaM :: (Traversable f, Monad m) => (a -> m (f a)) -> a -> m (Fix f)
-- anaM alg = go
--  where
--    go = fmap Fix . traverse go <=< alg

deriving instance Eq (Fix THTypeF)
deriving instance Ord (Fix THTypeF)
deriving instance Show (Fix THTypeF)
deriving instance Generic (Fix f)

instance Pretty (Fix THTypeF) where
  pretty = pretty . unFix

instance Pretty (THTypeF (Fix THTypeF)) where
  pretty = ppGeneric


-- | Concrete type constructor, not a variable.
newtype TypeName = TypeName { unTypeName :: Syn.Name }
  deriving (Eq, Ord, Show, Generic)

instance Pretty TypeName where
  pretty tn = "TypeName" <+> pretty (TH.pprint (unTypeName tn))

-- | Type variable.
newtype TypeVarName = TypeVarName { unTypeVarName :: Syn.Name }
  deriving (Eq, Ord, Show, Generic)

instance Pretty TypeVarName where
  pretty tvn = "TypeVarName" <+> pretty (TH.pprint (unTypeVarName tvn))

newtype ConstructorName = ConstructorName { unConstructorName :: Syn.Name }
  deriving (Eq, Ord, Show, Generic)

instance Pretty ConstructorName where
  pretty cn = "ConstructorName" <+> pretty (TH.pprint (unConstructorName cn))


type THType = Fix THTypeF

data THConstraint =
    THEqual THType THType
  | THOther
      TypeName -- ^ Class type constructor
      [THType] -- ^ Arguments
  deriving (Eq, Ord, Show, Generic)

instance Pretty THConstraint where
  pretty = ppGeneric

data THConstructorName =
    VanillaConstructor TypeName
  | InfixConstructor TypeName
  | UInfixConstructor TypeName
  | TupleConstructor !Int
  | UnboxedTupleConstructor !Int
  | UnboxedSumConstructor !Int
  | ArrowConstructor
  | ListConstructor
  | PromotedConstructor ConstructorName
  | PromotedTupleConstructor !Int
  | PromotedNilConstructor
  | PromotedConsConstructor
  deriving (Eq, Ord, Show, Generic)

instance Pretty THConstructorName where
  pretty = ppGeneric

data THTypeF a =
    TForall [TypeVarName] [THConstraint] a
  -- | AppT Type Type
  | TSig a Syn.Kind
  | TVar TypeVarName
  | -- | Thanks to using concrete type name we *know* that it's a valid type
    -- and it known, i.e. it's not a higher-kinded variable we're dealing with.
     TApply THConstructorName [a]
   -- | InfixT Type Name Type
   -- | UInfixT Type Name Type
   | TParens a
   -- | TStar
   -- | TConstraint
   | TLit Syn.TyLit
   deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

data THTranslationError =
    THTypeVarInConstructorPosition TypeVarName
  | THWildcardError
  | THStarTError
  | THConstraintTError
  | THEqualityTError
  | THLiteralAppliedToTypes (NonEmpty Syn.Type)
  | THEqualityTAppliedToTypes (NonEmpty Syn.Type)
  | THInvalidConstraint Syn.Type
  | THConstraintUnresolvedConstructorVariable TypeVarName
  deriving (Eq, Ord, Show, Generic)

instance Pretty THTranslationError where
  pretty = ppGeneric

getConstraint
  :: forall m. MonadError THTranslationError m
  => Syn.Type
  -> m THConstraint
getConstraint = go []
  where
    go :: [Syn.Type] -> Syn.Type -> m THConstraint
    go args typ = case typ of
      Syn.AppT (Syn.AppT Syn.EqualityT x) y ->
        case args of
          []     -> THEqual <$> toTHType x <*> toTHType y
          a : as -> throwError $ THEqualityTAppliedToTypes $ a :| as
      Syn.ForallT _bndrs _xt t -> go args t
      Syn.AppT f x             -> go (x : args) f
      Syn.SigT t _kind         -> go args t
      Syn.VarT      name       -> throwError $ THConstraintUnresolvedConstructorVariable $ TypeVarName name
      Syn.ConT      name       -> res (TypeName name) args
      Syn.PromotedT _          -> throwError $ THInvalidConstraint typ
      Syn.InfixT  t1 name t2   -> res (TypeName name) (t1 : t2 : args)
      Syn.UInfixT t1 name t2   -> res (TypeName name) (t1 : t2 : args)
      Syn.ParensT t            -> go args t
      Syn.TupleT{}             -> throwError $ THInvalidConstraint typ
      Syn.UnboxedTupleT{}      -> throwError $ THInvalidConstraint typ
      Syn.UnboxedSumT{}        -> throwError $ THInvalidConstraint typ
      Syn.ArrowT               -> throwError $ THInvalidConstraint typ
      Syn.EqualityT            -> throwError $ THInvalidConstraint typ
      Syn.ListT                -> throwError $ THInvalidConstraint typ
      Syn.PromotedTupleT{}     -> throwError $ THInvalidConstraint typ
      Syn.PromotedNilT         -> throwError $ THInvalidConstraint typ
      Syn.PromotedConsT        -> throwError $ THInvalidConstraint typ
      Syn.StarT                -> throwError $ THInvalidConstraint typ
      Syn.ConstraintT          -> throwError $ THInvalidConstraint typ
      Syn.LitT{}               -> throwError $ THInvalidConstraint typ
      Syn.WildCardT            -> throwError $ THInvalidConstraint typ
    res typ args = do
      args' <- traverse toTHType args
      pure $ THOther typ args'

toTHType
  :: forall m. MonadError THTranslationError m
  => Syn.Type
  -> m THType
toTHType = go []
  where
    go :: [Syn.Type] -> Syn.Type -> m THType
    go args = \case
      Syn.ForallT binders context t -> do
        constraints <- traverse getConstraint context
        Fix . TForall (map getBndrVar binders) constraints <$> go args t
      Syn.AppT f x           -> go (x : args) f
      Syn.SigT t kind        -> Fix <$> (TSig <$> go args t <*> pure kind)
      Syn.VarT      v        -> throwError $ THTypeVarInConstructorPosition $ TypeVarName v
      Syn.ConT      name     -> ret $ TApply (VanillaConstructor $ TypeName name)
      Syn.PromotedT name     -> ret $ TApply (PromotedConstructor $ ConstructorName name)
      Syn.InfixT  t1 name t2 -> retWithArgs (TApply $ InfixConstructor $ TypeName name) (t1 : t2 : args)
      Syn.UInfixT t1 name t2 -> retWithArgs (TApply $ UInfixConstructor $ TypeName name) (t1 : t2 : args)
      Syn.ParensT       t    -> go args t
      Syn.TupleT        n    -> ret $ TApply (TupleConstructor n)
      Syn.UnboxedTupleT n    -> ret $ TApply (UnboxedTupleConstructor n)
      Syn.UnboxedSumT   n    -> ret $ TApply (UnboxedSumConstructor n)
      Syn.ArrowT             -> ret $ TApply ArrowConstructor
      Syn.EqualityT          -> throwError THEqualityTError
      Syn.ListT              -> ret $ TApply ListConstructor
      Syn.PromotedTupleT n   -> ret $ TApply (PromotedTupleConstructor n)
      Syn.PromotedNilT       -> ret $ TApply PromotedNilConstructor
      Syn.PromotedConsT      -> ret $ TApply PromotedConsConstructor
      Syn.StarT              -> throwError THStarTError
      Syn.ConstraintT        -> throwError THConstraintTError
      Syn.LitT lit           ->
        case args of
          []   -> pure $ Fix $ TLit lit
          a:as -> throwError $ THLiteralAppliedToTypes $ a :| as
      Syn.WildCardT          -> throwError THWildcardError
      where
        ret cons = retWithArgs cons args
        retWithArgs cons args' = do
          args'' <- traverse (go []) args'
          pure $ Fix $ cons args''


fromTHType :: THType -> Syn.Type
fromTHType = undefined

newtype EqConstraints = EqConstraints
  { unEqConstraints :: Map TypeVarName THType }
  deriving (Generic)

instance Pretty EqConstraints where
  pretty = ppGeneric

instance Semigroup EqConstraints where
  (<>) (EqConstraints x) (EqConstraints y) =
    EqConstraints $ M.unionWithKey err x y
    where
      err var x' y' = error $ displayDocString $ ppDictHeader "Two equality constrains about the same variable"
        [ "variable" --> var
        , "value1"   --> x'
        , "value2"   --> y'
        ]

instance Monoid EqConstraints where
  mempty = EqConstraints M.empty
  mappend = (<>)


getBndrVar :: Syn.TyVarBndr -> TypeVarName
getBndrVar = \case
  Syn.PlainTV  name       -> TypeVarName name
  Syn.KindedTV name _kind -> TypeVarName name
