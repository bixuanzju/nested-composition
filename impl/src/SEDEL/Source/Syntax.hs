{-# LANGUAGE DeriveGeneric, MultiParamTypeClasses #-}

module SEDEL.Source.Syntax where

import SEDEL.Common

import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)

-- | Modules
data Module = Module
  { moduleEntries :: [SDecl]
  , mainExpr      :: SDecl
  } deriving (Show, Generic)

-- | Declarations other than traits
data SDecl
  = DefDecl TmBind
  | TypeDecl TypeBind
  deriving (Show, Generic)

type BindName = String

data Trait = TraitDef
  { selfType      :: (BindName, Type)
    -- ^ Self type
  , traitSuper    :: [Expr]
  , retType       :: Maybe Type
  , traitTyParams :: [(TyName, Type)]
  , traitParams   :: [(TmName, Type)]
  , traitBody     :: [TmBind]
  } deriving (Show, Generic)


-- f A1,...,An (x1: t1) ... (xn: tn): t = e
data TmBind = TmBind
  { bindName            :: BindName                  -- f
  , bindTyParams        :: [(TyName, Type)]          -- A1, ..., An
  , bindParams          :: [(TmName, Maybe Type)]    -- x1: t1, ..., xn: tn
  , bindRhs             :: Expr                      -- e
  , bindRhsTyAscription :: Maybe Type                -- t
  } deriving (Show, Generic)

-- type T[A1, ..., An] = t
data TypeBind = TypeBind
  { typeBindName   :: BindName           -- T
  , typeBindParams :: [(TyName, Kind)]   -- A1, ..., An
  , typeBindRhs    :: Type               -- t
  } deriving (Show, Generic)

-- Unbound library
type TmName = Name Expr
type TyName = Name Type

-- Expression
data Expr = Anno Expr Type
          | Var TmName
          | App Expr Expr
          | Lam (Bind TmName Expr)
          | Let (Bind (TmName, Embed Type) (Expr, Expr))
            -- ^ let expression, possibly recursive
          | DLam (Bind (TyName, Embed Type) Expr)
          | TApp Expr Type
          | DRec Label Expr
          | Acc Expr Label
          | Remove Expr Label Type
          | Merge Expr Expr
          | LitV Double
          | BoolV Bool
          | StrV String
          | PrimOp Operation Expr Expr
          | If Expr Expr Expr
          | Top
          | AnonyTrait Trait
          -- ^ Disappear after desugaring
          | DRec' TmBind
          -- ^ Disappear after desugaring
          | LamA (Bind (TmName, Embed Type) Expr)
          -- ^ Not exposed to users, for internal use
  deriving (Show, Generic)

type Label = String
data Type = NumT
          | BoolT
          | StringT
          | Arr Type Type
          | And Type Type
          | TVar TyName
          | DForall (Bind (TyName, Embed Type) Type)
          | SRecT Label Type
          | TopT
          | OpAbs (Bind (TyName, Embed Kind) Type)
          -- ^ Type-level abstraction: "type T A = t" becomes "type T = \A : *. t",
          | OpApp Type Type
          -- ^ Type-level application: t1 t2

  deriving (Show, Generic)

-- Kinds k := * | k -> k
data Kind = Star | KArrow Kind Kind deriving (Eq, Show, Generic)


-- Unbound library instances

instance Alpha Type
instance Alpha Expr
instance Alpha Trait
instance Alpha SDecl
instance Alpha TmBind
instance Alpha TypeBind
instance Alpha Kind

instance Subst Expr Type
instance Subst Expr Kind
instance Subst Expr ArithOp
instance Subst Expr LogicalOp
instance Subst Expr Operation
instance Subst Expr CompOp
instance Subst Expr Trait
instance Subst Expr SDecl
instance Subst Expr TmBind
instance Subst Expr TypeBind

instance Subst Expr Expr where
  isvar (Var v) = Just (SubstName v)
  isvar _ = Nothing

instance Subst Type Expr
instance Subst Type Trait
instance Subst Type Operation
instance Subst Type LogicalOp
instance Subst Type CompOp
instance Subst Type ArithOp
instance Subst Type SDecl
instance Subst Type TmBind
instance Subst Type TypeBind
instance Subst Type Kind

instance Subst Type Type where
  isvar (TVar v) = Just (SubstName v)
  isvar _ = Nothing
