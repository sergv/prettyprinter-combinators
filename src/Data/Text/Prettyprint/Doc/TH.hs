----------------------------------------------------------------------------
-- |
-- Module      :  Data.Text.Prettyprint.Doc.TH
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

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Data.Text.Prettyprint.Doc.TH
  ( derivePP
  , expandTypeSynsPP
  , testPP
  ) where

  -- ( testPP
  -- ) where

import Control.Arrow (first, second)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict hiding ((<>))

import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as CL8
import qualified Data.ByteString.Short as ShortBS
import Data.Coerce
import Data.Foldable
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Traversable
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Combinators as PP
import Data.Text.Prettyprint.Doc.Generics
import Data.Text.Prettyprint.Doc.TH.Utils
import Data.Text.Prettyprint.Doc.MetaDoc as MetaPP
-- import Data.Text.Prettyprint.Doc.Show
import Language.Haskell.TH
-- import Language.Haskell.TH.Lib

import Debug.Trace

-- | Pretty-print a type.
testPP
  :: HasCallStack
  => TypeQ -- ^ Name of type to derive function for.
  -> ExpQ
testPP typ = do
  typ' <- typ
  case toTHType typ' of
    Left err ->
      fail $ displayDocString $ ppDictHeader
        "Failed to obtain THType from input type"
        [ "error"  --> err
        , "type"   :-> ppGeneric typ'
        , "pprint" :-> pretty (pprint typ')
        ]
    Right thtyp ->
      fail $ displayDocString $ ppDictHeader
        "testPP"
        [ "type"   :-> ppGeneric typ'
        , "pprint" :-> pretty (pprint typ')
        , "THType" --> thtyp
        ]

expandTypeSynsPP
  :: HasCallStack
  => TypeQ -- ^ Name of type to derive function for.
  -> ExpQ
expandTypeSynsPP typ = do
  typ'     <- typ
  expanded <- expandTypeSynonyms typ'
  fail $ displayDocString $ ppDictHeader
    "testPP"
    [ "type"             :-> ppGeneric typ'
    , "pprint"           :-> pretty (pprint typ')
    , "expanded"         :-> ppGeneric expanded
    , "expanded(pprint)" :-> pretty (pprint expanded)
    ]


expandTypeSynonyms :: Type -> Q Type
expandTypeSynonyms = go
  where
    go typ = case typ of
      ForallT binders context t ->
        ForallT binders <$> traverse go context <*> go t
      AppT f x           -> AppT <$> go f <*> go x
      SigT t kind        -> SigT <$> go t <*> pure kind
      VarT{}             -> pure typ
      ConT{}             -> pure typ
      PromotedT{}        -> pure typ
      InfixT  t1 name t2 -> InfixT <$> go t1 <*> pure name <*> go t2
      UInfixT t1 name t2 -> UInfixT <$> go t1 <*> pure name <*> go t2
      ParensT t          -> ParensT <$> go t
      TupleT{}           -> pure typ
      UnboxedTupleT{}    -> pure typ
      UnboxedSumT{}      -> pure typ
      ArrowT             -> pure typ
      EqualityT          -> pure typ
      ListT              -> pure typ
      PromotedTupleT{}   -> pure typ
      PromotedNilT       -> pure typ
      PromotedConsT      -> pure typ
      StarT              -> pure typ
      ConstraintT        -> pure typ
      LitT{}             -> pure typ
      WildCardT          -> pure typ


-- newtype FunctionName = FunctionName { unFunctionName :: Name }
--   deriving (Eq, Ord, Show, Generic)
--
-- instance Pretty FunctionName where
--   pretty = viaShow
--
-- newtype PPType = PPType { unPPType :: Type }
--
-- instance Pretty PPType where
--   pretty = viaShow . pprint . unPPType


-- | Function of 1 argument.
newtype Function1 = Function1 { unFunction1 :: ExpQ }

instance Pretty Function1 where
  pretty = const "Function1"

applyFunction1 :: Function1 -> ExpQ -> ExpQ
applyFunction1 (Function1 f) = appE f


newtype GenState = GenState
  { gsPrinterCache :: Map (DecomposedType TypeName) Function1
  } deriving (Generic)

instance Pretty GenState where
  pretty = ppGeneric

type M = WriterT [DecQ] (StateT GenState Q)

liftQ :: Q a -> M a
liftQ = lift . lift

defaultPrinters :: HasCallStack => [(DecomposedType TypeName, Function1)]
defaultPrinters =
  [ (atomicType $ TypeName ''Int,                     Function1 [e| MetaPP.metaDocInt |])
  , (atomicType $ TypeName ''Double,                  Function1 [e| MetaPP.metaDocDouble |])
  , (atomicType $ TypeName ''String,                  Function1 [e| MetaPP.stringMetaDoc |])
  , (atomicType $ TypeName ''T.Text,                  Function1 [e| MetaPP.strictTextMetaDoc |])
  , (atomicType $ TypeName ''TL.Text,                 Function1 [e| MetaPP.lazyTextMetaDoc |])
  , (atomicType $ TypeName ''C8.ByteString,           Function1 [e| MetaPP.strictByteStringMetaDoc |])
  , (atomicType $ TypeName ''CL8.ByteString,          Function1 [e| MetaPP.lazyByteStringMetaDoc |])
  , (atomicType $ TypeName ''ShortBS.ShortByteString, Function1 [e| MetaPP.shortByteStringMetaDoc |])
  ]
  where
    atomicType :: a -> DecomposedType a
    atomicType tt = DecomposedType
      { dtConstructor = tt
      , dtArgs        = []
      , dtCxt         = []
      }

    -- mkPretty0 :: HasCallStack => Function1
    -- mkPretty0 = Function1 [e| pretty |]

  -- , (TopArrow,                    const $ pure $ Function1 [e| \_ -> pretty "<function>" |])

runM :: M a -> Q (a, [DecQ])
runM action =
  evalStateT (runWriterT action) initialState
  where
    initialState = GenState
      { gsPrinterCache = M.fromList defaultPrinters
      }


data TypeInfo = TypeInfo
  { tiCtx          :: Cxt
  , -- | Type name
    tiName         :: TypeName
  , tiBndrs        :: [TyVarBndr]
  , tiConstructors :: [Con]
  } deriving (Show)

getTypeInfo
  :: HasCallStack
  => TypeName -- ^ Type name, E.g. 'GHC.Types.Int'.
  -> Q TypeInfo
getTypeInfo typeName = do
  info <- reify $ unTypeName typeName
  dec  <- case info of
    TyConI  d            -> pure d
    FamilyI d _instances -> pure d
    unexpected           -> errorDoc $
      "Expected a type name but got:" ## ppGeneric unexpected
  case dec of
    DataD    cxt' typName tyVarBndrs _kind constructors _derivings -> pure TypeInfo
      { tiCtx          = cxt'
      , tiName         = TypeName typName
      , tiBndrs        = tyVarBndrs
      , tiConstructors = constructors
      }
    NewtypeD cxt' typName tyVarBndrs _kind constructor  _derivings -> pure TypeInfo
      { tiCtx          = cxt'
      , tiName         = TypeName typName
      , tiBndrs        = tyVarBndrs
      , tiConstructors = [constructor]
      }
    -- TySynD name tyVars typ -> undefined
    -- DataInstD -> undefined
    -- NewtypeInstD -> undefined
    unexpected -> errorDoc $
      -- /type synonym/data family/newtype family
      "Expected data/newtype declaration, but got:" ## ppGeneric unexpected

-- | Derive pretty function (a -> Doc ann) for a given type name.
derivePP
  :: HasCallStack
  => TypeQ -- ^ Name of type to derive function for.
  -> ExpQ
derivePP typ = do
  typ' <- typ
  (ppFunc, decs) <- runM $
    getPrinterForType typ' =<< liftQ (decomposeType typ')
  e <- letE decs [e| mdPayload . $(unFunction1 ppFunc) |]
  pure e


applyToTypes :: Name -> [TypeQ] -> TypeQ
applyToTypes typ =
  L.foldl' appT (conT typ)

applyToPVars :: Name -> [Name] -> PatQ
applyToPVars typ = conP typ . map varP

updateBndrVar :: TypeVarName -> TyVarBndr -> TyVarBndr
updateBndrVar (TypeVarName name) = \case
  PlainTV  _      -> PlainTV name
  KindedTV _ kind -> KindedTV name kind

-- | Type at the top of the application chain.
data ToplevelType =
    TopType !TypeName
  | TopTuple !Int
  | TopArrow
  | TopList
  deriving (Eq, Generic, Ord, Show)

instance Pretty ToplevelType where
  pretty = ppGeneric

data DecomposedType a = DecomposedType
  { dtConstructor :: a
  , dtArgs        :: [Type]
  , dtCxt         :: Cxt
  } deriving (Eq, Ord, Show, Generic, Functor, Foldable, Traversable)

instance Pretty a => Pretty (DecomposedType a) where
  pretty = ppGeneric

decomposeType :: HasCallStack => Type -> Q (DecomposedType ToplevelType)
decomposeType bigType = do
  -- traceM $ displayDocString $ ppDictHeader "decomposeType"
  --   [ "bigType" :-> ppGeneric bigType
  --   ]
  res <- runReaderT (go [] bigType) mempty
  traceM $ displayDocString $ ppDictHeader "decomposeType"
    [ "bigType" :-> ppGeneric bigType
    , "result"  --> res
    ]
  pure res
  where
    go :: [Type] -> Type -> ReaderT EqualityConstraints Q (DecomposedType ToplevelType)
    go acc = \case
      ForallT _bndrs context t -> do
        eqConstraints <- lift $ inferEqualityConstraints context
        traceM $ displayDocString $ ppDictHeader "decomposeType.go"
          [ "bigType"       :-> pretty (pprint bigType)
          , "eqConstraints" --> eqConstraints
          ]
        updateCxt <$> local (eqConstraints <>) (go acc t)
        where
          updateCxt dt = dt
            { dtCxt =
                filter (\typ -> not $ S.null $ getFreeTypeVarsSet typ `S.intersection` varsInResType) $ context ++ dtCxt dt
            }
            where
              varsInResType = foldMap getFreeTypeVarsSet $ dtArgs dt
      typ@(VarT name)  -> do
        eqConstraints <- ask
        typ' <- lift $ applyEqualityConstraints eqConstraints typ
        case typ' of
          VarT _ ->
            errorDoc $ ppDictHeader
            "Encountered type variable in a constructor position. *Check that 'derivePP' was not applied to a polymorphic type."
            [ "variable"      :-> viaShow name
            , "eqConstraints" --> eqConstraints
            , "type"          :-> ppGeneric bigType
            ]
          _ -> go acc typ'
      ConT name        -> mkRes acc $ TopType $ TypeName name
      AppT x y         -> go (y : acc) x
      InfixT x name y  -> mkRes (x : y : acc) $ TopType $ TypeName name
      UInfixT x name y -> mkRes (x : y : acc) $ TopType $ TypeName name
      ParensT x        -> go acc x
      TupleT n         -> mkRes acc $ TopTuple n
      UnboxedTupleT n  -> mkRes acc $ TopType $ TypeName $ unboxedTupleTypeName n
      UnboxedSumT n    -> mkRes acc $ TopType $ TypeName $ unboxedSumTypeName n
      ArrowT           -> mkRes acc TopArrow
      ListT            -> mkRes acc TopList
      unsupported      -> errorDoc $
        "Cannot get type name from a type" <+> ppGeneric unsupported <+> "within:" ## ppGeneric bigType
      where
        mkRes args con = do
          eqConstraints <- ask
          args' <- lift $ traverse (applyEqualityConstraints eqConstraints) args
          pure DecomposedType
            { dtConstructor = con
            , dtArgs        = args'
            , dtCxt         = []
            }

getPrinterForType
  :: HasCallStack
  => Type -- ^ Original type for prettyprining of messages.
  -> DecomposedType ToplevelType
  -> M Function1
getPrinterForType origType dt@DecomposedType{dtConstructor} =
  case dtConstructor of
    TopType name -> do
      let dt' = name <$ dt
      cached <- gets (M.lookup dt' . gsPrinterCache)
      cache <- get
      case cached of
        Just f  -> do
          traceM $ displayDocString $ ppDictHeader "getPrinterForType/cached"
            [ "origType"        :-> pretty (pprint origType)
            , "decomposed type" :-> pretty dt'
            , "cached"          :-> ppGeneric cached
            ]
          pure f
        Nothing -> do
          traceM $ displayDocString $ ppDictHeader "getPrinterForType/uncached"
            [ "origType"        :-> pretty (pprint origType)
            , "decomposed type" :-> pretty dt'
            , "cache"           :-> pretty cache
            ]
          genPPForKindStar dt'
    -- TopVar  name -> do
    --   traceM $ displayDocString $ ppDictHeader "getPrinterForType/TopVar"
    --     [ "dt"   --> dt
    --     , "name" --> name
    --     ]
    --   genPPForKindStar dt $ unTypeVarName name
    TopArrow     -> pure $ Function1
      [e| \_ -> MetaPP.stringMetaDoc $(stringE $ "<function : " ++ pprint origType ++ ">") |]
    TopList      -> genPPForList dt
    TopTuple _   -> genPPForTuple dt

mkClause :: HasCallStack => Map TypeVarName Type -> Cxt -> Con -> M (PatQ, ExpQ)
mkClause tenv tenvCxt = \case
  ForallC _bndrs forallCxt con -> mkClause tenv (tenvCxt ++ forallCxt) con
  NormalC conName fields  -> do
    traceM $ displayDocString $ ppDictHeader
      "mkClause"
      [ "tenv"    :-> PP.ppMapWith pretty ppGeneric tenv
      , "tenvCxt" :-> ppListWith ppGeneric tenvCxt
      ]
    patVars <- for fields $ \(_strictness, typ) -> do
      name    <- liftQ $ newName "x"
      let typ' = substitute tenv typ
      decomposed <- liftQ $ do
        eqConstraints <- inferEqualityConstraints tenvCxt
        decomposeType =<< applyEqualityConstraints eqConstraints typ'
      printer    <- getPrinterForType typ' decomposed
      pure (printer, name)
    let body :: ExpQ
        body =
          [e| MetaPP.constructorAppMetaDoc
                (MetaPP.stringMetaDoc $(stringE (pprint conName)))
                $(listE $ flip map patVars $ \(printer, fieldVar) ->
                   [e| $(applyFunction1 printer (varE fieldVar)) |])
            |]
          -- [e| PP.ppDictHeader
          --       (PP.pretty $(stringE (pprint conName)))
          --       $(listE $ flip map patVars $ \(typ, name) ->
          --         [e| $(stringE (pprint )) PP.:-> x |])
          --   |]
    pure (applyToPVars conName $ map snd patVars, body)
  unsupported ->
    errorDoc $ "Unsupported constructor:" ## ppGeneric unsupported
  -- RecC Name [VarBangType]            --   C { v :: Int, w :: a }
  -- InfixC BangType Name BangType      --   Int :+ a
  -- GadtC [Name] [BangType] Type       --   C :: a -> b -> T b Int
  -- RecGadtC [Name] [VarBangType] Type -- C :: { v :: Int } -> T b Int

-- | Generate prettyprinter for lists applied to specific types.
genPPForList
  :: (HasCallStack, Pretty a)
  => DecomposedType a
  -> M Function1
genPPForList dt@DecomposedType{dtArgs} = do
  arg <- case dtArgs of
    [x]     -> pure x
    invalid -> errorDoc $ ppDictHeader
      ("Expected list constructor applied to single type argument, but got" <+> pretty (length invalid) <+> "arguments")
      [ "arguments"       :-> ppGeneric invalid
      , "decomposed type" --> dt
      ]
  printer <- getPrinterForType arg =<< liftQ (decomposeType arg)
  pure $ Function1 [e| MetaPP.atomicMetaDoc . PP.ppListWith (MetaPP.mdPayload . $(unFunction1 printer)) |]

-- | Generate prettyprinter for tuples applied to specific types.
genPPForTuple
  :: HasCallStack
  => DecomposedType a
  -> M Function1
genPPForTuple DecomposedType{dtArgs} = do
  printers <- traverse (\typ -> getPrinterForType typ =<< liftQ (decomposeType typ)) dtArgs
  case printers of
   [p] -> pure p -- Print unit tuples without any parens.
   ps  -> do
     ps' <- liftQ $ traverse (\printer -> (printer, ) <$> newName "x") ps
     pure $ Function1 $ lamE [tupP $ map (varP . snd) ps'] $
       [e| MetaPP.atomicMetaDoc
             (PP.ppTupleWith MetaPP.mdPayload $(listE (map (\(p, var) -> unFunction1 p `appE` varE var) ps'))) |]


genPPForKindStar
  :: HasCallStack
  => DecomposedType TypeName -- ^ Type parameters of the constructor & context for variables within type parameters.
  -> M Function1             -- ^ Function that can print values of specified type.
genPPForKindStar dt@DecomposedType{dtConstructor, dtArgs, dtCxt} = do
  let typeParams  = dtArgs
      typeContext = dtCxt
  TypeInfo{tiName, tiBndrs, tiConstructors} <- liftQ $ getTypeInfo dtConstructor
  tenv <- if length typeParams == length tiBndrs
          then pure $ M.fromList $ zip (map getBndrVar tiBndrs) typeParams
          else errorDoc $ ppDictHeader
            "Expected saturated type of kind 'Star' but got underappliedtype"
            [ "constructor" :-> ppGeneric dtConstructor
            , "type params" :-> ppGeneric typeParams
            ]
  -- Generate new declaration and register it
  let -- base = nameBase $ unTypeName tiName
      name = "pp_" ++ map (\c -> if c == '.' then '_' else c) (pprint (unTypeName tiName))
             -- maybe base (\x -> x ++ "_" ++ base) (nameModule $ unTypeName tiName)
  funcName <- liftQ $ newName name
  annTVar  <- liftQ $ newName "ann"

  -- Register printer here so that recursive printing will resolve
  -- to the function we're defining currently.
  let res :: Function1
      res = Function1 (varE funcName)
  modify $ \s -> s { gsPrinterCache = M.insert dt res $ gsPrinterCache s }

  bigArg  <- liftQ $ newName "x"
  clauses <- traverse (mkClause tenv typeContext) tiConstructors
  let sig :: DecQ
      sig = sigD funcName $ addQuantifiers (pure typeContext) $ -- forallT (PlainTV annTVar : tiBndrs) (pure tiCtx) $
              arrowT
                `appT` applyToTypes (unTypeName dtConstructor) (map pure typeParams) -- (map getBndrVar tiBndrs)
                `appT` applyToTypes ''MetaPP.MetaDoc [varT annTVar]
      cases :: [MatchQ]
      cases = map (\(pat, body) -> match pat (normalB body) []) clauses
      fun :: DecQ
      fun = funD funcName $ (:[]) $
              clause
                [varP bigArg]
                (normalB (caseE (varE bigArg) cases))
                []
  -- sig' <- liftQ $ sig
  -- fail $ displayDocString $ ppDictHeader "genPPForKindStar"
  --   [ "dt"  :-> pretty dt
  --   , "sig" :-> ppGeneric sig'
  --   ]

  tell [sig, fun]
  pure res


newtype EqualityConstraints = EqualityConstraints
  { unEqualityConstraints :: Map TypeVarName Type }
  deriving (Generic)

instance Pretty EqualityConstraints where
  pretty = ppGeneric

instance Semigroup EqualityConstraints where
  (<>) (EqualityConstraints x) (EqualityConstraints y) =
    EqualityConstraints $ M.unionWithKey err x y
    where
      err var x' y' = error $ displayDocString $ ppDictHeader "Two equality constrains about the same variable"
        [ "variable" --> var
        , "value1"   :-> ppGeneric x'
        , "value2"   :-> ppGeneric y'
        ]

instance Monoid EqualityConstraints where
  mempty = EqualityConstraints M.empty
  mappend = (<>)

applyEqualityConstraints :: EqualityConstraints -> Type -> Q Type
applyEqualityConstraints constraints initType = go 10000 initType
  where
    go 0 typ = fail $ displayDocString $ ppDictHeader
      "Infinite loop while applying equality constraints"
      [ "initialType" :-> ppGeneric initType
      , "type so far" :-> ppGeneric typ
      , "constraints" --> constraints
      ]

    go n typ = do
      let (typ', updateOccured) = coerce substitute' constraints typ
      if updateOccured
        then go (n - 1) typ'
        else pure typ'

inferEqualityConstraints :: Cxt -> Q EqualityConstraints
inferEqualityConstraints = flip execStateT mempty . traverse_ go
  where
    addConstraint :: TypeVarName -> Type -> StateT EqualityConstraints Q ()
    addConstraint var typ = do
      oldBinding <- gets (M.lookup var . unEqualityConstraints)
      case oldBinding of
        Nothing          -> modify $ coerce (M.insert var typ)
        Just (VarT name) ->
          -- Go down the binding chain and add this constraint at the end.
          -- Chain should have no cycles thanks to ingenious implementation
          -- of function 'inferEqualityConstraints.go'.
          addConstraint (TypeVarName name) typ
        Just t           ->
          unless (t == typ) $
            fail $ displayDocString $ ppDictHeader
              "Unsatisfiable equality constraints: variable must be equal to two distinct values at the same time"
              [ "variable" --> var
              , "type1"    :-> ppGeneric t
              , "type2"    :-> ppGeneric typ
              ]
    go :: Type -> StateT EqualityConstraints Q ()
    go = \case
      ForallT _bndrs context t -> traverse_ go context *> go t
      AppT (AppT EqualityT x) y ->
        case (x, y) of
          (VarT x', VarT y') -> do
            xMapping <- gets (M.lookup (TypeVarName x') . unEqualityConstraints)
            case xMapping of
              Just _  -> pure ()
              Nothing -> do
                yMapping <- gets (M.lookup (TypeVarName y') . unEqualityConstraints)
                case yMapping of
                  Just _  -> pure ()
                  Nothing -> addConstraint (TypeVarName x') y
          (VarT x', _)       ->
            addConstraint (TypeVarName x') y
          (_,       VarT y') ->
            addConstraint (TypeVarName y') x
          (_,       _)       -> fail $ displayDocString $ ppDictHeader
            "Too complicated equality constraint"
            [ "lhs" :-> ppGeneric x
            , "rhs" :-> ppGeneric y
            ]
      AppT f x          -> go f *> go x
      SigT t _kind      -> go t
      VarT _            -> pure ()
      ConT _            -> pure ()
      PromotedT _       -> pure ()
      InfixT t1 _ t2    -> go t1 *> go t2
      UInfixT t1 _ t2   -> go t1 *> go t2
      ParensT t         -> go t
      TupleT{}          -> pure ()
      UnboxedTupleT{}   -> pure ()
      UnboxedSumT{}     -> pure ()
      ArrowT            -> pure ()
      EqualityT         -> pure ()
      ListT             -> pure ()
      PromotedTupleT{}  -> pure ()
      PromotedNilT      -> pure ()
      PromotedConsT     -> pure ()
      StarT             -> pure ()
      ConstraintT       -> pure ()
      LitT{}            -> pure ()
      WildCardT         -> pure ()


getFreeTypeVarsSet :: Type -> Set TypeVarName
getFreeTypeVarsSet = S.fromList . getFreeTypeVars

-- | Get type variables not qualified in a type.
getFreeTypeVars :: Type -> [TypeVarName]
getFreeTypeVars typ' = go typ' []
  where
    go :: Type -> [TypeVarName] -> [TypeVarName]
    go typ acc = case typ of
      ForallT binders _context t ->
        filter (`S.notMember` boundVars) $ go t acc
        where
          boundVars = S.fromList (map getBndrVar binders)
      AppT f x         -> go f $ go x acc
      SigT t _kind     -> go t acc
      VarT v           -> TypeVarName v : acc
      ConT _           -> mempty
      PromotedT _      -> mempty
      InfixT t1 _ t2   -> go t1 $ go t2 acc
      UInfixT t1 _ t2  -> go t1 $ go t2 acc
      ParensT t        -> go t acc
      TupleT{}         -> mempty
      UnboxedTupleT{}  -> mempty
      UnboxedSumT{}    -> mempty
      ArrowT           -> mempty
      EqualityT        -> mempty
      ListT            -> mempty
      PromotedTupleT{} -> mempty
      PromotedNilT     -> mempty
      PromotedConsT    -> mempty
      StarT            -> mempty
      ConstraintT      -> mempty
      LitT{}           -> mempty
      WildCardT        -> mempty

substitute :: Map TypeVarName Type -> Type -> Type
substitute substEnv substTyp = fst $ substitute' substEnv substTyp

-- | Returns updated type an whether any substitution has taken place.
substitute':: Map TypeVarName Type -> Type -> (Type, Bool)
substitute' substEnv substTyp =
  second getAny $ runReader (runWriterT (go substTyp)) substEnv
  where
    go :: (MonadReader (Map TypeVarName Type) m, MonadWriter Any m) => Type -> m Type
    go typ = case typ of
      ForallT binders context t ->
        local (`M.restrictKeys` boundVars) $
          ForallT binders <$> traverse go context <*> go t
        where
          boundVars = S.fromList (map getBndrVar binders)
      AppT f x         -> AppT <$> go f <*> go x
      SigT t kind      -> SigT <$> go t <*> pure kind
      VarT v           -> do
        v' <- asks (M.lookup (TypeVarName v))
        case v' of
          Just t  -> do
            tell $ Any True
            pure t
          Nothing -> pure typ
      ConT{}           -> pure typ
      PromotedT{}      -> pure typ
      InfixT t1 name t2  -> InfixT <$> go t1 <*> pure name <*> go t2
      UInfixT t1 name t2 -> UInfixT <$> go t1 <*> pure name <*> go t2
      ParensT t        -> ParensT <$> go t
      TupleT{}         -> pure typ
      UnboxedTupleT{}  -> pure typ
      UnboxedSumT{}    -> pure typ
      ArrowT           -> pure typ
      EqualityT        -> pure typ
      ListT            -> pure typ
      PromotedTupleT{} -> pure typ
      PromotedNilT     -> pure typ
      PromotedConsT    -> pure typ
      StarT            -> pure typ
      ConstraintT      -> pure typ
      LitT{}           -> pure typ
      WildCardT        -> pure typ

-- | Refresh all forall'ed variable names so that they won't clash with
-- anything.
refreshType :: Type -> Q Type
refreshType = flip runReaderT mempty . go
  where
    go :: Type -> ReaderT (Map TypeVarName Type) Q Type
    go typ = case typ of
      ForallT binders context t -> do
        (refreshEnv, binders') <- lift $ fmap (first M.fromList . unzip) $ for binders $ \b -> do
          let oldName = getBndrVar b
          freshName <- newName $ nameBase $ unTypeVarName oldName
          typB <- varT freshName
          pure ((oldName, typB), updateBndrVar (TypeVarName freshName) b)

        local (M.union refreshEnv) $
          ForallT binders' <$> traverse go context <*> go t
      AppT f x         -> AppT <$> go f <*> go x
      SigT t kind      -> SigT <$> go t <*> pure kind
      VarT v           -> do
        v' <- asks (M.lookup (TypeVarName v))
        case v' of
          Just t  -> pure t
          Nothing -> pure typ
      ConT{}           -> pure typ
      PromotedT{}      -> pure typ
      InfixT t1 name t2  -> InfixT <$> go t1 <*> pure name <*> go t2
      UInfixT t1 name t2 -> UInfixT <$> go t1 <*> pure name <*> go t2
      ParensT t        -> ParensT <$> go t
      TupleT{}         -> pure typ
      UnboxedTupleT{}  -> pure typ
      UnboxedSumT{}    -> pure typ
      ArrowT           -> pure typ
      EqualityT        -> pure typ
      ListT            -> pure typ
      PromotedTupleT{} -> pure typ
      PromotedNilT     -> pure typ
      PromotedConsT    -> pure typ
      StarT            -> pure typ
      ConstraintT      -> pure typ
      LitT{}           -> pure typ
      WildCardT        -> pure typ

addQuantifiers :: CxtQ -> TypeQ -> TypeQ
addQuantifiers context typ = do
  typ' <- typ
  forallT (map (PlainTV . unTypeVarName) $ ordNub $ getFreeTypeVars typ') context (pure typ')

-- | Like 'ordNub' and 'Data.List.nubBy'. Selects a key for each element and
-- takes the nub based on that key.
ordNub :: Ord a => [a] -> [a]
ordNub = go S.empty
  where
    go !_ [] = []
    go !s (x:xs)
      | x `S.member` s = go s xs
      | otherwise        = let !s' = S.insert x s
                           in x : go s' xs


errorDoc :: HasCallStack => PP.Doc ann -> a
errorDoc = error . displayDocString
