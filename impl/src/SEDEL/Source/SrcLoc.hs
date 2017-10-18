{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module SEDEL.Source.SrcLoc where

-- import Text.PrettyPrint.ANSI.Leijen

type Located a = GenLocated Loc a

data GenLocated l e = L l e
                    deriving (Eq, Ord, Show)

instance Functor (GenLocated l) where
  fmap f (L l e) = L l (f e)

deriving instance Foldable    (GenLocated l)
deriving instance Traversable (GenLocated l)

data Loc
  = Loc { line :: !Int
       ,  column :: !Int}
  | NoLoc
  deriving (Eq, Ord, Show)

-- instance Pretty Loc where
--     pretty (Loc l c) = int l <> colon <> int c <> colon
--     pretty NoLoc = empty

unLoc :: Located a -> a
unLoc (L _ x) = x

withLoc :: b -> Located a -> Located b
x `withLoc` (L loc _) = L loc x

noLoc :: a -> Located a
noLoc = L NoLoc

withLocs :: b -> [Located a] -> Located b
withLocs x [] = noLoc x
withLocs x (l:_) = x `withLoc` l
