{-# LANGUAGE FlexibleContexts, PatternGuards, NoImplicitPrelude, LambdaCase, OverloadedStrings #-}

module SEDEL.Source.Typing
  ( tcModule
  ) where

import qualified Data.Map as M
import           Protolude hiding (Type)
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>), (<$>), Pretty)
import           Unbound.Generics.LocallyNameless


import           SEDEL.Common
import           SEDEL.Environment
import           SEDEL.PrettyPrint
import           SEDEL.Source.Desugar
import           SEDEL.Source.Subtyping
import           SEDEL.Source.Syntax
import qualified SEDEL.Target.CBN as TC
import qualified SEDEL.Target.Syntax as T
import           SEDEL.Util


-- Type check a module
tcModule :: Module -> TcMonad (Type, T.UExpr, TC.Env)
tcModule m = do
  let decls = moduleEntries m
  let mainE = mainExpr m
  -- Step 1: Desugar traits
  let sdecls = desugar (decls ++ [mainE])
  -- Step 2: Check module
  targetDecls <- foldr tcM (return []) sdecls
  -- Step 3: Generate initial environment for execution
  let (mainType, mainTarget) =
        maybe (TopT, (s2n "main", T.UUnit)) identity (lastMay targetDecls)
  let declsTarget = fmap (map snd) (initMay targetDecls)
  let initEnv =
        maybe
          TC.emptyEnv
          (foldl' (\env (n, e) -> TC.extendCtx (n, e, env) env) TC.emptyEnv)
          declsTarget
  return (mainType, snd mainTarget, initEnv)
  where
    tcM ::
         SDecl
      -> TcMonad [(Type, (T.UName, T.UExpr))]
      -> TcMonad [(Type, (T.UName, T.UExpr))]
    tcM (DefDecl decl) ms = do
      (dbind, transD) <- tcTmDecl decl
      ((snd dbind, transD) :) <$>
        localCtx (uncurry extendVarCtx dbind) ms
    tcM (TypeDecl tdecl) ms = do
      (n, tdef, k) <- tcTyDecl tdecl
      localCtx (addTypeSynonym n tdef k) ms

-- Type check declarations
tcTmDecl :: TmBind -> TcMonad ((TmName, Type), (T.UName, T.UExpr))
tcTmDecl decl =
  lookupTmDef (s2n n) >>= \case
    Nothing -> do
      (typ, tran) <- infer term
      return ((s2n n, typ), (s2n n, tran))
    Just _ -> throwError $ text "Multiple definitions of" <+> text n
  where
    (n, term) = normalizeTmDecl decl -- term has been annotated, so we can infer

tcTyDecl :: TypeBind -> TcMonad (TyName, Type, Kind)
tcTyDecl (TypeBind n params rhs) = do
  let typDef = pullRight params rhs
  return (s2n n, typDef, Star)

-- | Kinding.
kind :: Fresh m => Ctx -> Type -> m (Maybe Kind)
kind d (TVar a) = return $ lookupTVarKindMaybe d a
kind _ NumT = return $ Just Star
kind _ BoolT = return $ Just Star
kind _ StringT = return $ Just Star
kind _ TopT = return $ Just Star
kind d (Arr t1 t2) = justStarIffAllHaveKindStar d [t1, t2]
kind d (And t1 t2) = justStarIffAllHaveKindStar d [t1, t2]
kind d (DForall b) = do
  ((a, _), t) <- unbind b
  kind (extendTVarCtx a Star d) t
kind d (SRecT _ t) = justStarIffAllHaveKindStar d [t]

{-
    Δ,x::k1 ⊢ t :: k
    -------------------- (K-Abs)
    Δ ⊢ λx:k1. t :: k1 -> k
-}
kind d (OpAbs b) = do
  ((x, Embed k1), t) <- unbind b
  maybe_k <- kind (extendTVarCtx x k1 d) t
  case maybe_k of
    Nothing -> return Nothing
    Just k  -> return $ Just (KArrow k1 k)

{-
    Δ ⊢ t1 :: k11 -> k12  Δ ⊢ t2 :: k11
    ------------------------------------ (K-App)
    Δ ⊢ t1 t2 :: k12
-}
kind d (OpApp t1 t2) = do
  maybe_k1 <- kind d t1
  maybe_k2 <- kind d t2
  case (maybe_k1, maybe_k2) of
    (Just (KArrow k11 k12), Just k2)
      | k2 == k11 -> return (Just k12)
    _ -> return Nothing



justStarIffAllHaveKindStar :: Fresh m => Ctx -> [Type] -> m (Maybe Kind)
justStarIffAllHaveKindStar d ts = do
  ps <- mapM (hasKindStar d) ts
  if and ps
    then return $ Just Star
    else return Nothing

hasKindStar :: Fresh m => Ctx -> Type -> m Bool
hasKindStar d t = do
  k <- kind d t
  return (k == Just Star)



-- | "Pull" the type params at the LHS of the equal sign to the right.
-- A (high-level) example:
--   A B t  ->  \A. \B. t
pullRight :: [(TyName, Kind)] -> Type -> Type
pullRight params t = foldr (\(n, k) t' -> OpAbs (bind (n, embed k) t')) t params





---------------------------
-- Γ ⊢ e ⇒ A ~> E

-- note: target is untyped
---------------------------

infer :: Expr -> TcMonad (Type, T.UExpr)

{-

Γ ⊢ ⊤ ⇒ ⊤  ~> ()

-}
infer Top = return (TopT, T.UUnit)

infer (LitV n) = return (NumT, T.ULitV n)

infer (BoolV b) = return (BoolT, T.UBoolV b)

infer (StrV b) = return (StringT, T.UStrV b)

{-

   x:A ∈ Γ
---------------
Γ ⊢ x ⇒ A ~> x

-}
infer (Var x) = do
  t <- lookupVarTy x
  return (t, T.UVar (translate x)) -- Change the sort of a name

{-

Γ ⊢ e ⇐ A  ~> E
------------------
Γ ⊢ e : A ⇒ A ~> E

-}
infer (Anno e a) = do
  c <- askCtx
  let a' = expandType c a
  e' <- tcheck e a'
  return (a, e')

{-

Γ ⊢ e1 ⇒ A1 -> A2  ~> E1
Γ ⊢ e2 ⇐ A1        ~> E2
----------------------------
Γ ⊢ e1 e2 ⇒ A2     ~> E1 E2

-}
infer inp@(App e1 e2) = do
  (arr, e1') <- infer e1
  c <- askCtx
  case expandType c arr of
    Arr a1 a2 -> do
      e2' <- tcheck e2 a1
      return (a2, T.UApp e1' e2')
    _ ->
      throwError
        (hang 2 $
         text "type of application mismatch in" <+>
         squotes (pprint inp) PP.<> colon PP.<$> text "function" <+>
         squotes (pprint e1) <+> text "has type" <+> squotes (pprint arr))

{-

Γ ⊢ e ⇒ ∀(α ∗ B). C  ~> E
Γ ⊢ A
Γ ⊢ A ∗ B
-------------------------------
Γ ⊢ e A ⇒ [α := A] C  ~> E

-}
infer inp@(TApp e a) = do
  wf a
  (t, e') <- infer e
  ctx <- askCtx
  case expandType ctx t of
    DForall t' -> do
      ((x, Embed b), c) <- unbind t'
      disjoint ctx (expandType ctx a) (expandType ctx b)
      return (subst x a c, e')
    _ ->
      throwError
        (hang 2 $
         text "type of application mismatch in" <+>
         squotes (pprint inp) PP.<> colon PP.<$> text "type-level function" <+>
         squotes (pprint e) <+> text "has type" <+> squotes (pprint t))

{-

Γ ⊢ e1 ⇒ A ~> E1
Γ ⊢ e2 ⇒ B ~> E2
Γ ⊢ A∗B
------------------------------
Γ ⊢ e1,,e2 ⇒ A&B ~> (E1, E2)

-}
infer (Merge e1 e2) = do
  (a, e1') <- infer e1
  (b, e2') <- infer e2
  ctx <- askCtx
  disjoint ctx (expandType ctx a) (expandType ctx b)
  return (And a b, T.UPair e1' e2')

{-

Γ ⊢ e ⇒ A ~> E
----------------------
Γ ⊢{l=e} ⇒ {l:A} ~> E

-}
infer (DRec l e) = do
  (a, e') <- infer e
  return (SRecT l a, e')

{-

Γ ⊢ e ⇒ {l : A} ~> E
----------------------
Γ ⊢ e.l ⇒ A ~> E

The above is what is shown in the paper. In the implementation, we'd like to
avoid annotating a record before projection. The following is the modified rule:

Γ ⊢ e ⇒ t ~> E
t • l = t1 ~> c
-----------------------
Γ ⊢ e.l ⇒ t1 ~> c E

-}

-- ad-hoc extension of toString method
infer (Acc e "toString") = do
  (_, e') <- infer e
  return (StringT, T.UToString e')

infer (Acc e "sqrt") = do
  e' <- tcheck e NumT
  return (NumT, T.USqrt e')

infer (Acc e l) = do
  (t, e') <- infer e
  ctx <- askCtx
  case select (expandType ctx t) l of
    [(a, c)] -> return (a, T.UApp c e')
    _ ->
      throwError
        (hang 2 $
         text "expect a record type with label" <+>
         squotes (text l) <+>
         text "for" <+>
         squotes (pprint e) PP.<$> text "but got" <+> squotes (pprint t))


{-


Γ ⊢ e ⇒ t ~> E
t \ l = t1 ~> c
-----------------------
Γ ⊢ e.l ⇒ t1 ~> c E

-}

infer (Remove e l lt) = do
  (t, e') <- infer e
  ctx <- askCtx
  let t' = expandType ctx t
  case restrict t' l lt of
    Just (a, c) -> return (a, T.UApp c e')
    _ ->
      throwError
        (hang 2 $
         text "expect a record type with label" <+>
         squotes (text l) <+>
         text "for" <+>
         squotes (pprint e) PP.<$> text "but got" <+> squotes (pprint t))




{-

Γ ⊢ A
Γ , a * A ⊢ e ⇒ B ~> E
a fresh
---------------------------------
Γ ⊢ Λ(α∗A).e ⇒ ∀(α∗A).B ~> E

-}
infer (DLam t) = do
  ((x, Embed a), e) <- unbind t
  wf a
  (b, e') <- localCtx (extendConstrainedTVarCtx x a) $ infer e
  return (DForall (bind (x, embed a) b), e')

infer (PrimOp op e1 e2) =
  case op of
    Arith _ -> do
      e1' <- tcheck e1 NumT
      e2' <- tcheck e2 NumT
      return (NumT, T.UPrimOp op e1' e2')
    Logical _ -> do
      e1' <- tcheck e1 BoolT
      e2' <- tcheck e2 BoolT
      return (BoolT, T.UPrimOp op e1' e2')
    Comp cop | cop == Equ || cop == Neq -> do
      let res1 = do
            e1' <- tcheck e1 NumT
            e2' <- tcheck e2 NumT
            return (e1', e2')
          res2 = do
            e1' <- tcheck e1 StringT
            e2' <- tcheck e2 StringT
            return (e1', e2')
          res3 = do
            e1' <- tcheck e1 BoolT
            e2' <- tcheck e2 BoolT
            return (e1', e2')
      (e1', e2') <- res1 <|> res2 <|> res3
      return (BoolT, T.UPrimOp op e1' e2')
    Comp _ -> do
      e1' <- tcheck e1 NumT
      e2' <- tcheck e2 NumT
      return (BoolT, T.UPrimOp op e1' e2')
    Append -> do
      e1' <- tcheck e1 StringT
      e2' <- tcheck e2 StringT
      return (StringT, T.UPrimOp op e1' e2')

infer inp@(If e1 e2 e3) = do
  e1' <- tcheck e1 BoolT
  (t2, e2') <- infer e2
  (t3, e3') <- infer e3
  ctx <- askCtx
  let t2' = expandType ctx t2
  let t3' = expandType ctx t3
  if aeq t2' t3'
    then return (t2, T.UIf e1' e2' e3')
    else throwError
         (hang 2 $
          text "if branches type mismatch in" <+>
          squotes (pprint inp) PP.<> colon PP.<$> squotes (pprint e2) <+>
          text "has type" <+>
          squotes (pprint t2) PP.<$> squotes (pprint e3) <+>
          text "has type" <+> squotes (pprint t3))

{-

Γ, x:t ⊢ e1 ⇐ t ~> e1'
Γ, x:t ⊢ e2 ⇒ t' ~> e2'
-----------------------------------------------------
Γ ⊢ let x : t = e1 in e2 ⇒ t' ~> let x = e1' in e2'

Note: Recursive let binding

-}
infer (Let b) = do
  ((x, Embed t), (e1, e2)) <- unbind b
  e1' <- localCtx (extendVarCtx x t) $ tcheck e1 t
  (t', e2') <- localCtx (extendVarCtx x t) $ infer e2
  return (t', T.ULet (bind (translate x) (e1', e2')))



infer (LamA t) = do
  ((x, Embed a), e) <- unbind t
  wf a
  (b, e') <- localCtx (extendVarCtx x a) $ infer e
  return (Arr a b, T.ULam (bind (translate x) e'))



infer a = throwError $ text "Infer not implemented:" <+> pprint a




------------------------
-- Γ ⊢ e ⇐ A ~> E
------------------------

tcheck :: Expr -> Type -> TcMonad T.UExpr

{-

Γ ⊢ A
Γ , x:A ⊢ e ⇐ B ~> E
---------------------------
Γ ⊢ λx. e ⇐ A → B ~> λx. E

-}
tcheck (Lam l) (Arr a b) = do
  (x, e) <- unbind l
  wf a
  e' <- localCtx (extendVarCtx x a) $ tcheck e b
  return (T.ULam (bind (translate x) e'))

{-


Γ ⊢ A
Γ , a * A ⊢ e ⇐ B ~> E
---------------------------------
Γ ⊢ Λ(α∗A).e ⇐ ∀(α∗A).B ~> E


-}
tcheck (DLam l) (DForall b) =
  unbind2 l b >>= \case
    Just ((x, Embed a), e, _, t') -> do
      wf a
      localCtx (extendConstrainedTVarCtx x a) $ tcheck e t'
    Nothing -> throwError $ text "Patterns have different binding variables"

{-

Γ ⊢ e1 ⇐ A ~> E1
Γ ⊢ e2 ⇐ B ~> E2
Γ ⊢ A∗B
------------------------------
Γ ⊢ e1,,e2 ⇐ A&B ~> (E1, E2)

-}
tcheck e@(Merge e1 e2) b@(And a' b') = do
  let re2 = do
        e1' <- tcheck e1 a'
        e2' <- tcheck e2 b'
        ctx <- askCtx
        let aa = expandType ctx a'
        let bb = expandType ctx b'
        disjoint ctx aa bb
        return (T.UPair e1' e2')
  let re1 = do
        wf b
        (a, e') <- infer e
        ctx <- askCtx
        let res = subtype ctx a b
        case res of
          Right c -> return (T.UApp c e')
          Left str -> do
            throwError $
              (hang 2 $
               text "subtyping failed" PP.<> colon PP.<$> squotes (pprint e) <+>
               text "has type" <+>
               squotes (pprint a) PP.<$> text "which is not a subtype of" <+>
               squotes (pprint b))
  re2 <|> re1



{-

Γ ⊢ e ⇐ A ~> E
----------------------
Γ ⊢{l=e} ⇐ {l:A} ~> E

-}

tcheck (DRec l e) (SRecT l' a) = do
  when (l /= l') $
    throwError (text "Labels not equal" <+> text l <+> text "and" <+> text l')
  tcheck e a



{-

Γ ⊢ e ⇒ t ~> E
t • l = B ~> c
B <: A ~> c'
-----------------------
Γ ⊢ e.l ⇐ A ~> c' (c E)

-}

-- ad-hoc extension of toString method
tcheck (Acc e "toString") StringT = do
  (_, e') <- infer e
  return (T.UToString e')

tcheck (Acc e "sqrt") NumT = do
  e' <- tcheck e NumT
  return (T.USqrt e')

tcheck (Acc e l) a = do
  (t, e') <- infer e
  ctx <- askCtx
  let ls = select (expandType ctx t) l
  case length ls of
    0 ->
      throwError
        (hang 2 $
         text "expect a record type with label" <+>
         squotes (text l) <+>
         text "for" <+>
         squotes (pprint e) PP.<$> text "but got" <+> squotes (pprint t))
    -- Multiple label of 'l' are found, find the only one whose type is a subtype of 'a'
    _ ->
      let (bs, cs) = unzip ls
          res = dropWhile (isLeft . fst) $ zip (fmap (flip (subtype ctx) a) bs) cs
      in case res of
           (Right c', c):_ -> return $ T.UApp c' (T.UApp c e')
           _ ->
             throwError
               (hang 2 $
                text "Cannot find a subtype of" <+>
                squotes (pprint a) <+>
                text "for label" <+>
                text l PP.<$> text "in" <+> squotes (pprint e))


{-

Γ ⊢ e ⇒ A ~> E
A <: B ~> c
Γ ⊢ B
-------------------
Γ ⊢ e ⇐ B ~> c E

-}

tcheck e b = do
  wf b
  (a, e') <- infer e
  ctx <- askCtx
  let res = subtype ctx a b
  case res of
    Right c -> return (T.UApp c e')
    Left str ->
      throwError $
        (hang 2 $
         text "subtyping failed" PP.<> colon PP.<$> squotes (pprint e) <+>
         text "has type" <+>
         squotes (pprint a) PP.<$> text "which is not a subtype of" <+>
         squotes (pprint b))


-- | Check that a (expanded) type is well-formed: disjoint and has kind *.
wf :: Type -> TcMonad ()
wf t = do
  ctx <- askCtx
  let t' = expandType ctx t
  maybe_kind <- kind ctx t'
  case maybe_kind of
    Nothing -> throwError $ squotes (pprint t) <+> text "is not well-kinded"
    Just Star -> wf' t'
    Just k ->
      throwError
        (hang 2 $
         text "expect type" <+>
         squotes (pprint t) <+>
         text "has kind star" PP.<$> text "but got" <+> squotes (pprint k))


wf' :: Type -> TcMonad ()
wf' NumT = return ()
wf' BoolT = return ()
wf' StringT = return ()
wf' (Arr a b) = wf' a >> wf' b
wf' (And a b) = do
  wf' a
  wf' b
wf' (TVar x) = void $ lookupTVarConstraint x
wf' (DForall t) = do
  ((x, Embed a), b) <- unbind t
  wf' a
  localCtx (extendConstrainedTVarCtx x a) $ wf' b
wf' (SRecT _ t) = wf' t
wf' TopT = return ()
wf' t = throwError $ text "type" <+> pprint t <+> text "is not well-formed"

-------------------------------------------------------------------------
-- WARN: This is the most critical component!!!
--
-- Anything new added in the types, we should double check how it
-- affects the disjointess relation
-------------------------------------------------------------------------

disjoint :: (Fresh m, MonadError Doc m) => Ctx -> Type -> Type -> m ()
disjoint _ TopT _ = return ()
disjoint _ _ TopT = return ()

disjoint ctx (TVar x) b
  | Just a <- lookupTVarConstraintMaybe ctx x
  , Right _ <- subtype ctx a b = return ()
disjoint ctx b (TVar x)
  | Just a <- lookupTVarConstraintMaybe ctx x
  , Right _ <- subtype ctx a b = return ()
disjoint _ (TVar x) (TVar y) =
  throwError $
  text "Type variables:" <+>
  text (name2String x) <+>
  text "and" <+> text (name2String y) <+> text "are not disjoint"

disjoint ctx (DForall t) (DForall t') =
  unbind2 t t' >>= \case
    Just ((x, Embed a1), b, (_, Embed a2), c) ->
      disjoint (extendConstrainedTVarCtx x (And a1 a2) ctx) b c
    _ -> throwError $ text "Patterns have different binding variables"

disjoint ctx (SRecT l a) (SRecT l' b) =
  when (l == l') $ disjoint ctx a b

disjoint ctx (Arr _ a2) (Arr _ b2) = disjoint ctx a2 b2
disjoint ctx (And a1 a2) b = do
  disjoint ctx a1 b
  disjoint ctx a2 b
disjoint ctx a (And b1 b2) = do
  disjoint ctx a b1
  disjoint ctx a b2
disjoint _ a b =
  unless (disjointAx a b) $
  throwError $ pprint a <+> text "is not disjoint with" <+> pprint b


disjointAx :: Type -> Type -> Bool
disjointAx t1 t2 =
  type2num t1 < 6 && type2num t2 < 6 && type2num t1 /= type2num t2
  where
    type2num :: Type -> Int
    type2num NumT = 0
    type2num BoolT = 1
    type2num StringT = 2
    type2num Arr {} = 3
    type2num DForall {} = 4
    type2num SRecT {} = 5
    -- The above are basic type
    type2num TopT {} = 6
    type2num And {} = 7
    type2num TVar {} = 8
    type2num OpAbs {} = 9
    type2num OpApp {} = 10





--------------------
-- τ1 • l = τ2  ~> C
--------------------

-- | Select a label l from t
-- Return a list of possible types with their projections
select :: Type -> Label -> [(Type, T.UExpr)]
select t l =
  fromMaybe [] (M.lookup l m)
  where
    m = recordFields t

recordFields :: Type -> Map Label [(Type, T.UExpr)]
recordFields = go identity
  where
    go :: (T.UExpr -> T.UExpr) -> Type -> Map Label [(Type, T.UExpr)]
    go cont (And t1 t2) =
      M.unionWith (++) (go (T.UP1 . cont) t1) (go (T.UP2 . cont) t2)
    go cont (SRecT l' t') =
      M.fromList [(l', [(t', T.elam "x" (cont (T.evar "x")))])]
    go _ _ = M.empty


----------------------
-- τ1 \ l = τ2 ~> C
----------------------

restrict :: Type -> Label -> Type -> Maybe (Type, T.UExpr)
restrict t l lt = go t
  where
    go (SRecT l' t') = if l == l' && aeq lt t' then Just (TopT, T.elam "x" T.UUnit) else Nothing
    go (And t1 t2) =
      let m1 = go t1
          m2 = go t2
          m1' = fmap (\(tt, c) -> (And tt t2, T.elam "x" (T.UPair (T.UApp c (T.UP1 (T.evar "x"))) (T.UP2 (T.evar "x"))))) m1
          m2' = fmap (\(tt, c) -> (And t1 tt, T.elam "x" (T.UPair (T.UP1 (T.evar "x")) (T.UApp c (T.UP2 (T.evar "x")))))) m2
      in m1' <|> m2'
    go _ = Nothing
