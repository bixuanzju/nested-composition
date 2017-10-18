{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module SEDEL.Source.Subtyping
  ( subtype
  ) where


import           Data.Sequence ((|>), ViewL(..))
import qualified Data.Sequence as Q
import           Protolude hiding (Type)
import           Text.PrettyPrint.ANSI.Leijen hiding (Pretty, (<$>))
import           Unbound.Generics.LocallyNameless

import           SEDEL.Environment
import           SEDEL.PrettyPrint
import           SEDEL.Source.Syntax
import           SEDEL.Source.Desugar
import qualified SEDEL.Target.Syntax as T


data L = LTy Type | LLa Label

----------------------------
-- Coercion
----------------------------

type Co = T.UExpr


coId :: Co
coId = T.elam "x" (T.evar "x")

coTrans :: Co -> Co -> Co
coTrans c1 c2 = T.elam "x" (T.eapp c1 (T.eapp c2 (T.evar "x")))


coTop1 :: Co
coTop1 = T.elam "x" T.UUnit

coTop2 :: Co
coTop2 = T.elam "x" (T.elam "y" T.UUnit)

coArr :: Co -> Co -> Co
coArr c1 c2 =
  let body = T.eapp c2 (T.eapp (T.evar "f") (T.eapp c1 (T.evar "x")))
  in T.elam "f" (T.elam "x" body)


coPair :: Co -> Co -> Co
coPair c1 c2 =
  T.elam "x" (T.UPair (T.eapp c1 (T.evar "x")) (T.eapp c2 (T.evar "x")))


coProj1 :: Co
coProj1 = T.elam "x" (T.UP1 (T.evar "x"))


coProj2 :: Co
coProj2 = T.elam "x" (T.UP2 (T.evar "x"))


coDist :: Co
coDist = T.elam "x" (T.elam "y" $ T.UPair
                     (T.eapp (T.UP1 (T.evar "x")) (T.evar "y"))
                     (T.eapp (T.UP2 (T.evar "x")) (T.evar "y")))

----------------------------
-- Metafunction
----------------------------

calTop :: Q.Seq L -> Co
calTop (Q.null -> True) = coTop1
calTop (Q.viewl -> LLa _ :< fs) = coTrans (calTop fs) coId
calTop (Q.viewl -> LTy _ :< fs) =
  coTrans (coArr coId (calTop fs)) (coTrans (coArr coTop1 coTop1) (coTrans coTop2 coTop1))
calTop _ = error "Impossible happened in calTop"

calAnd :: Q.Seq L -> Co
calAnd (Q.null -> True) = coId
calAnd (Q.viewl -> LLa _ :< fs) = coTrans (calAnd fs) coId
calAnd (Q.viewl -> LTy _ :< fs) = coTrans (coArr coId (calAnd fs)) coDist
calAnd _ = error "Impossible happened in calAnd"


----------------------------
-- A <: B ~> E
-- note: target is untyped
----------------------------
-- | Subtyping (<:) is defined only between types of kind *.
-- WARN: They must be expanded first
subtype :: Ctx -> Type -> Type -> Either Doc T.UExpr
subtype ctx st tt = runExcept $ runFreshMT go
  where
    go :: (FreshMT (Except Doc)) T.UExpr
    go = do
      let a = expandType ctx st
      let b = expandType ctx tt
      subtypeS Q.empty a b

    subtypeS :: Q.Seq L -> Type -> Type -> (FreshMT (Except Doc)) T.UExpr

    -- Base cases

    subtypeS (Q.null -> True) NumT NumT = return coId

    subtypeS (Q.null -> True) BoolT BoolT = return coId

    subtypeS (Q.null -> True) StringT StringT = return coId

    subtypeS fs _ TopT = return $ coTrans (calTop fs) coTop1

    subtypeS (Q.null -> True) (TVar a) (TVar b) =
      if a /= b
        then throwError $
             text "variables not equal:" <+>
             text (show a) <+> text "and" <+> text (show b)
        else return coId

    subtypeS (Q.null -> True) (DForall t1) (DForall t2) =
      unbind2 t1 t2 >>= \case
        Just ((_, Embed a1), b1, (_, Embed a2), b2) ->
          subtypeS Q.empty a2 a1 >> subtypeS Q.empty b1 b2
        Nothing ->
          throwError $ text "Patterns have different binding variables"

    -- NumT
    subtypeS fs (And a1 a2) NumT = do
      let c1 = do
            c <- subtypeS fs a1 NumT
            return $ coTrans c coProj1
          c2 = do
            c <- subtypeS fs a2 NumT
            return $ coTrans c coProj2
      c1 <|> c2


    subtypeS (Q.viewl -> LTy a :< fs) (Arr a1 a2) NumT = do
      c1 <- subtypeS Q.empty a a1
      c2 <- subtypeS fs a2 NumT
      return $ coArr c1 c2

    subtypeS (Q.viewl -> LLa l :< fs) (SRecT l' a) NumT =
      if l == l'
      then subtypeS fs a NumT
      else throwError $ text "Labels: " <+> text l <+> text "and" <+> text l' <+> "not equal"


    -- BoolT
    subtypeS fs (And a1 a2) BoolT = do
      let c1 = do
            c <- subtypeS fs a1 BoolT
            return $ coTrans c coProj1
          c2 = do
            c <- subtypeS fs a2 BoolT
            return $ coTrans c coProj2
      c1 <|> c2


    subtypeS (Q.viewl -> LTy a :< fs) (Arr a1 a2) BoolT = do
      c1 <- subtypeS Q.empty a a1
      c2 <- subtypeS fs a2 BoolT
      return $ coArr c1 c2

    subtypeS (Q.viewl -> LLa l :< fs) (SRecT l' a) BoolT =
      if l == l'
      then subtypeS fs a BoolT
      else throwError $ text "Labels: " <+> text l <+> text "and" <+> text l' <+> "not equal"


    -- StringT
    subtypeS fs (And a1 a2) StringT = do
      let c1 = do
            c <- subtypeS fs a1 StringT
            return $ coTrans c coProj1
          c2 = do
            c <- subtypeS fs a2 StringT
            return $ coTrans c coProj2
      c1 <|> c2


    subtypeS (Q.viewl -> LTy a :< fs) (Arr a1 a2) StringT = do
      c1 <- subtypeS Q.empty a a1
      c2 <- subtypeS fs a2 StringT
      return $ coArr c1 c2

    subtypeS (Q.viewl -> LLa l :< fs) (SRecT l' a) StringT =
      if l == l'
      then subtypeS fs a StringT
      else throwError $ text "Labels: " <+> text l <+> text "and" <+> text l' <+> "not equal"


    -- Alpha
    subtypeS fs (And a1 a2) (TVar x) = do
      let c1 = do
            c <- subtypeS fs a1 (TVar x)
            return $ coTrans c coProj1
          c2 = do
            c <- subtypeS fs a2 (TVar x)
            return $ coTrans c coProj2
      c1 <|> c2


    subtypeS (Q.viewl -> LTy a :< fs) (Arr a1 a2) (TVar x) = do
      c1 <- subtypeS Q.empty a a1
      c2 <- subtypeS fs a2 (TVar x)
      return $ coArr c1 c2

    subtypeS (Q.viewl -> LLa l :< fs) (SRecT l' a) (TVar x) =
      if l == l'
      then subtypeS fs a (TVar x)
      else throwError $ text "Labels: " <+> text l <+> text "and" <+> text l' <+> "not equal"



    -- Forall
    subtypeS fs (And a1 a2) (DForall t) = do
      let c1 = do
            c <- subtypeS fs a1 (DForall t)
            return $ coTrans c coProj1
          c2 = do
            c <- subtypeS fs a2 (DForall t)
            return $ coTrans c coProj2
      c1 <|> c2


    subtypeS (Q.viewl -> LTy a :< fs) (Arr a1 a2) (DForall t) = do
      c1 <- subtypeS Q.empty a a1
      c2 <- subtypeS fs a2 (DForall t)
      return $ coArr c1 c2

    subtypeS (Q.viewl -> LLa l :< fs) (SRecT l' a) (DForall t) =
      if l == l'
      then subtypeS fs a (DForall t)
      else throwError $ text "Labels: " <+> text l <+> text "and" <+> text l' <+> "not equal"




    -- Inductive cases

    subtypeS fs a (And b1 b2) = do
      c1 <- subtypeS fs a b1
      c2 <- subtypeS fs a b2
      return $ coTrans (calAnd fs) (coPair c1 c2)


    subtypeS fs a (Arr b1 b2) = do
      subtypeS (fs |> LTy b1) a b2


    subtypeS fs a (SRecT l b) = do
      subtypeS (fs |> LLa l) a b

    subtypeS fs a b =
      throwError $
      text "Invalid subtyping:" <+>
      squotes (pprint a) <+> text "and" <+> squotes (pprint b)

