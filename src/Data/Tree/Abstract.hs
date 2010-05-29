{-# LANGUAGE
    DeriveFunctor
  , DeriveFoldable
  , DeriveTraversable
  , TemplateHaskell
  , EmptyDataDecls
  , TypeFamilies
  #-}
module Data.Tree.Abstract where

import Data.Binary
import Data.Foldable
import Data.Traversable
import Data.Fixpoint
import Generics.Regular hiding (Fix (..))
import qualified Generics.Regular.Functions.Binary as G

-- | Binary search tree datatype parametrized with key/value types and
-- recursive positions.

data TreeF k v f = Leaf | Branch k v f f
  deriving ( Eq, Ord, Show
           , Functor, Foldable, Traversable
           )

type Tree k v = Fix (TreeF k v)

-- Derive generic representation using Regular.

$(deriveAll ''TreeF "PFTree")
type instance PF (TreeF k v f) = PFTree k v f

-- Binary instance, we get this for free using the generic binary function.

instance (Binary k, Binary v, Binary f) => Binary (TreeF k v f) where
  put = G.gput
  get = G.gget

-- Destructor.

tree :: r -> (k -> v -> f -> f -> r) -> TreeF k v f -> r
tree l _ Leaf             = l
tree _ b (Branch k v l r) = b k v l r

-- Smart constructors.

leaf :: Tree k v
leaf = In Leaf

branch :: k -> v -> Tree k v -> Tree k v -> Tree k v
branch k v l r = In (Branch k v l r)

