{-# LANGUAGE OverloadedStrings, FlexibleContexts, NoImplicitPrelude #-}

module SEDEL.Environment
  ( lookupVarTy
  , lookupTVarConstraint
  , lookupTVarConstraintMaybe
  , lookupTVarSynMaybe
  , lookupTmDef
  , lookupTVarKindMaybe
  , runTcMonad
  , TcMonad
  , M
  , askCtx
  , localCtx
  , extendVarCtx
  , extendTVarCtx
  , extendVarCtxs
  , extendConstrainedTVarCtx
  , addTypeSynonym
  , addTypeSynonyms
  , Ctx(..)
  , emptyCtx
  ) where


import qualified Data.Map.Strict as M
import           Protolude hiding (Type)
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty, (<$>))
import Unbound.Generics.LocallyNameless

import           SEDEL.Source.Syntax


type M a = FreshMT (ReaderT a (Except Doc))

type TcMonad = M Ctx

runTcMonad :: Ctx -> TcMonad a -> Either Doc a
runTcMonad env m = runExcept $ runReaderT (runFreshMT m) env

-- | `TypeValue` is what's put inside a type context.
data TypeValue
  = TerminalType
  -- ^ Terminal types, e.g., the `a` of `forall a. `
  | NonTerminalType Type
    -- ^ Non-terminal types, i.e. type synoyms. `Type` holds the RHS to the
    -- equal sign of type synonym definitions.

type VarCtx = M.Map TmName Type
type BndCtx = M.Map TmName Expr
type TyCtx
   = M.Map TyName ( Kind
                  , Type
                  -- ^ disjointness Constraint
                  , TypeValue)

-- | Environment manipulation and accessing functions
data Ctx = Ctx
  { varCtx :: VarCtx
  , tyCtx :: TyCtx
  , bndCtx :: BndCtx
  }


askCtx :: TcMonad Ctx
askCtx = ask

localCtx :: (Ctx -> Ctx) -> TcMonad a -> TcMonad a
localCtx = local

emptyCtx :: Ctx
emptyCtx = Ctx {varCtx = M.empty, tyCtx = M.empty, bndCtx = M.empty}

ctxMap :: (VarCtx -> VarCtx)
       -> (TyCtx -> TyCtx)
       -> (BndCtx -> BndCtx)
       -> Ctx
       -> Ctx
ctxMap f1 f2 f3 ctx =
  Ctx
  {varCtx = f1 (varCtx ctx), tyCtx = f2 (tyCtx ctx), bndCtx = f3 (bndCtx ctx)}

extendVarCtx :: TmName -> Type -> Ctx -> Ctx
extendVarCtx v t = ctxMap (M.insert v t) identity identity

extendTVarCtx :: TyName -> Kind -> Ctx -> Ctx
extendTVarCtx v k = ctxMap identity (M.insert v (k, TopT, TerminalType)) identity

extendConstrainedTVarCtx :: TyName -> Type -> Ctx -> Ctx
extendConstrainedTVarCtx v t = ctxMap identity (M.insert v (Star, t, TerminalType)) identity

extendVarCtxs :: [(TmName, Type)] -> Ctx -> Ctx
extendVarCtxs = flip $ foldr (uncurry extendVarCtx)

addTypeSynonym :: TyName -> Type -> Kind -> Ctx -> Ctx
addTypeSynonym v t k = ctxMap identity (M.insert v (k, t, NonTerminalType t)) identity

addTypeSynonyms :: [(TyName, Type, Kind)] -> Ctx -> Ctx
addTypeSynonyms = flip $ foldr (\(v, t, k) ctx -> addTypeSynonym v t k ctx)

lookupVarTy
  :: (MonadReader Ctx m, MonadError Doc m)
  => TmName -> m Type
lookupVarTy v = do
  env <- asks varCtx
  case M.lookup v env of
    Nothing  -> throwError $ text "Not in scope:" <+> text (show v)
    Just res -> return res

lookupTVarConstraint
  :: (MonadReader Ctx m, MonadError Doc m)
  => TyName -> m Type
lookupTVarConstraint v = do
  env <- asks tyCtx
  case M.lookup v env of
    Nothing  -> throwError $ text "Not in scope:" <+> text (show v)
    Just (_, c, _) -> return c

lookupTVarKindMaybe :: Ctx -> TyName -> Maybe Kind
lookupTVarKindMaybe ctx v =  (\(k, _, _) -> k) <$> M.lookup v (tyCtx ctx)

lookupTVarConstraintMaybe :: Ctx -> TyName -> Maybe Type
lookupTVarConstraintMaybe ctx v =
  (\(_, t, _) -> t) <$> M.lookup v (tyCtx ctx)

lookupTVarSynMaybe :: Ctx -> TyName -> Maybe Type
lookupTVarSynMaybe ctx v =
  case (\(_, _, t) -> t) <$> M.lookup v (tyCtx ctx) of
    Nothing -> Nothing
    Just TerminalType -> Nothing
    Just (NonTerminalType t) -> Just t

lookupTmDef
  :: (MonadReader Ctx m)
  => TmName -> m (Maybe Expr)
lookupTmDef v = do
  env <- asks bndCtx
  return $ M.lookup v env
