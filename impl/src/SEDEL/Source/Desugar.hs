{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}


module SEDEL.Source.Desugar
  ( desugar
  , desugarExpr
  , resolveDecls
  , normalizeTmDecl
  , expandType
  ) where

import Protolude hiding (Type)
import Unbound.Generics.LocallyNameless

import SEDEL.Environment
import SEDEL.Source.Syntax
import SEDEL.Util

desugar :: [SDecl] -> [SDecl]
desugar = map go
  where
    go :: SDecl -> SDecl
    go (DefDecl decl) = DefDecl $ desugarTmBind decl
    go ty = ty

desugarTmBind :: TmBind -> TmBind
desugarTmBind b = b {bindRhs = desugarExpr (bindRhs b)}

desugarExpr :: Expr -> Expr
desugarExpr = runFreshM . go
  where
    go :: Fresh m => Expr -> m Expr
    go (Anno e t) = do
      e' <- go e
      return $ Anno e' t
    go (App e1 e2) = do
      e1' <- go e1
      e2' <- go e2
      return $ App e1' e2'
    go (Lam t) = do
      (n, body) <- unbind t
      body' <- go body
      return $ Lam (bind n body')
    go (DLam b) = do
      ((n, t), body) <- unbind b
      body' <- go body
      return $ DLam (bind (n, t) body')
    go (TApp e t) = do
      e' <- go e
      return $ TApp e' t
    go (DRec l e) = do
      e' <- go e
      return $ DRec l e'
    go (Acc e l) = do
      e' <- go e
      return $ Acc e' l
    go (Remove e l t) = do
      e' <- go e
      return $ Remove e' l t
    go (Merge e1 e2) = do
      e1' <- go e1
      e2' <- go e2
      return $ Merge e1' e2'
    go (PrimOp op e1 e2) = do
      e1' <- go e1
      e2' <- go e2
      return $ PrimOp op e1' e2'
    go (If e1 e2 e3) = do
      e1' <- go e1
      e2' <- go e2
      e3' <- go e3
      return $ If e1' e2' e3'
    go (LamA b) = do
      ((n, t), body) <- unbind b
      body' <- go body
      return $ LamA (bind (n, t) body')
    go (AnonyTrait t) = return $ desugarTrait t
    go (DRec' b) =
      let (l, e) = normalizeTmDecl (desugarTmBind b)
      in return $ DRec l e
    go e = return e



-- Desugar trait
--
-- trait (x : A, y : B) inherits b & c {self : C => ...}
-- \(x : A) (y : B) (self : C) . b(self) ,, c(self) ,, {...}
desugarTrait :: Trait -> Expr
desugarTrait trait =
  normalize
    typarams
    (map (second Just) params ++ [(s2n self, Just st)])
    -- if no supers, return body otherwise merge them to body
    (maybe body (flip Merge body) (foldl1May Merge supers))
    (retType trait)
  where
    typarams = traitTyParams trait
    params = traitParams trait
    tb = map desugarTmBind (traitBody trait)
    (self, st) = selfType trait
    supers = traitSuper trait
    body = mkRecds (map normalizeTmDecl tb)



-- After parsing, earlier declarations appear first in the list
-- Substitute away all type declarations
resolveDecls :: [SDecl] -> [TmBind]
resolveDecls decls = map (substs substPairs) [decl | (DefDecl decl) <- decls]
  where
    tydecls =
      foldl'
        (\ds t -> substs (toSubst ds) t : ds)
        []
        [decl | decl@TypeDecl {} <- decls]
    substPairs = toSubst tydecls
    toSubst ds = [(s2n n, t) | TypeDecl (TypeBind n _ t) <- ds]

{-

Translate

def n [(A, T1), (B, T2)] [(x, A), (y, B)] C e

to

(n, /\ A*T1. B*T2. \x : A .\y : B . (e : C), [T1, T2])

-}


normalizeTmDecl :: TmBind -> (BindName, Expr)
normalizeTmDecl decl =
  ( bindName decl
  , normalize
      (bindTyParams decl)
      (bindParams decl)
      (bindRhs decl)
      (bindRhsTyAscription decl))

normalize :: [(TyName, Type)] -> [(TmName, Maybe Type)] -> Expr -> Maybe Type -> Expr
normalize tyParams params e ret = body
  where
    body = foldr (\(n, s) tm -> DLam (bind (n, Embed s) tm)) fun tyParams
    fun =
      foldr
        (\(n, t) tm ->
           case t of
             Just t' -> LamA (bind (n, Embed t') tm)
             Nothing -> Lam (bind n tm))
        (maybe e (Anno e) ret)
        params


-- | Recursively expand all type synonyms. The given type must be well-kinded.
expandType :: Ctx -> Type -> Type
expandType ctx ty = runFreshM (go ctx ty)
  where
    go :: Ctx -> Type -> FreshM Type
    -- Interesting cases:
    go d (TVar a) =
      case lookupTVarSynMaybe d a of
        Nothing -> return $ TVar a
        Just t -> go d t
    go d (OpAbs b) = do
      ((x, Embed k), t) <- unbind b
      t' <- go (extendTVarCtx x k d) t
      return $ OpAbs (bind (x, embed k) t')
    go d typ@(OpApp t1 t2) =
      go d t1 >>= \case
        OpAbs b -> do
          t2' <- go d t2
          ((x, Embed _), t) <- unbind b
          go d (subst x t2' t)
        _ -> return typ
    go _ NumT = return NumT
    go _ BoolT = return BoolT
    go _ StringT = return StringT
    go d (Arr t1 t2) = do
      t1' <- go d t1
      t2' <- go d t2
      return $ Arr t1' t2'
    go d (And t1 t2) = do
      t1' <- go d t1
      t2' <- go d t2
      return $ And t1' t2'
    go d (DForall b) = do
      ((a, Embed t1), t2) <- unbind b
      t1' <- go d t1
      t2' <- go d t2
      return $ DForall (bind (a, embed t1') t2')
    go d (SRecT l t) = do
      t' <- go d t
      return $ SRecT l t'
    go _ TopT = return TopT
