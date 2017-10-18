module SEDEL.Util where

import SEDEL.Source.Syntax

import Data.List (foldl', foldl1')
import Data.Maybe (fromMaybe)
import Unbound.Generics.LocallyNameless
import Unbound.Generics.LocallyNameless.Name

-- | Change the sort of a name.
translate :: Name a -> Name b
translate (Fn x y) = Fn x y
translate (Bn x y) = Bn x y

-- | 'unzip' transforms a list of pairs into a list of first components
-- and a list of second components.
unzip    :: [(a,b)] -> ([a],[b])
{-# INLINE unzip #-}
unzip    =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])



-- Utility for parsing

evar :: String -> Expr
evar = Var . s2n

tvar :: String -> Type
tvar = TVar . s2n

ebind :: String -> Expr -> Bind TmName Expr
ebind n = bind (s2n n)

elam :: String -> Expr -> Expr
elam b e = Lam (ebind b e)

dlam :: (String, Type) -> Expr -> Expr
dlam (s, t) b = DLam (bind (s2n s, embed t) b)

tforall :: (String,  Type) -> Type -> Type
tforall (s, t) b = DForall (bind (s2n s, embed t) b)

eapp :: Expr -> Expr -> Expr
eapp = App

etapp :: Expr -> Type -> Expr
etapp = TApp

mkRecds :: [(Label, Expr)] -> Expr
mkRecds [] = Top
mkRecds ((l, e):r) = foldl' (\t (l', e') -> Merge t (DRec l' e')) (DRec l e) r

mkRecds' :: [TmBind] -> Expr
mkRecds' = foldl1' Merge . map DRec'

mkRecdsT :: [(Label, Type)] -> Type
mkRecdsT [] = TopT
mkRecdsT ((l, e):r) = foldl (\t (l', e') -> And t (SRecT l' e')) (SRecT l e) r

mkArr :: Type -> [Type] ->Type
mkArr = foldr Arr

mkForall :: Type -> [(TyName, Embed Type)] -> Type
mkForall = foldr (\b t -> DForall (bind b t))

elet :: String -> Type -> Expr -> Expr -> Expr
elet s t e b = Let (bind (s2n s, embed t) (e, b))

transNew :: Type -> [Expr] -> Expr
transNew t es = elet "self" t (foldl1' Merge es) (evar "self")


{-

Translate

[(A, T1), (B, T2)] [(x, A), (y, B)] C e

to

\/ A*T1. B*T2. A -> B -> C

and

/\ A*T1. B*T2. \x.\y.e

-}

teleToTmBind ::
     [(String, Type)] -> [(String, Maybe Type)] -> Type -> Expr -> (Type, Expr)
-- Ideally for defrec, users should annotate all arguments, but here we assume T
-- if not annotated
teleToTmBind tys tms res e =
  let arr = foldr (\(_, t) tt -> Arr (fromMaybe TopT t) tt) res tms
      tbind = foldr tforall arr tys
      fun = foldr (\(n, _) tm -> elam n tm) e tms
      bfun = foldr dlam fun tys
  in (tbind, bfun)
