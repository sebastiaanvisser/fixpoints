{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , TemplateHaskell
  , EmptyDataDecls
  , TypeFamilies
  #-}
module Data.List.Abstract where

import Data.Binary
import Data.Foldable
import Data.Traversable
import Data.Fixpoint
import Generics.Regular hiding (Fix (..))
import qualified Generics.Regular.Functions.Binary as G

-- List datatype parametrized with value type and recursive positions.

data ListF x f = Nil | Cons x f
  deriving ( Eq, Ord, Show
           , Functor, Foldable, Traversable
           )

type List x = Fix (ListF x)

-- Derive generic representation using Regular.

$(deriveAll ''ListF "PFList")
type instance PF (ListF x f) = PFList x f

-- Binary instance, we get this for free using the generic binary function.

instance (Binary x, Binary f) => Binary (ListF x f) where
  put = G.gput
  get = G.gget

-- Destructor.

list :: r -> (x -> f -> r) -> ListF x f -> r
list n _ Nil         = n
list _ c (Cons x xs) = c x xs

-- Smart constructors.

nil :: List x
nil = In Nil

cons :: x -> List x -> List x
cons x xs = In (Cons x xs)

