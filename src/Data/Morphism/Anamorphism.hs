{-# LANGUAGE
    GADTs
  , FlexibleContexts
  , RankNTypes
  , KindSignatures
  #-}
module Generics.Morphism.Ana where

import Annotation.Annotation
import Control.Applicative
import Control.Monad.Identity
import Data.Traversable
import Generics.Fixpoint
import qualified Generics.Morphism.Apo as Apo

-- | Anamorphic coalgebra with type variables for annotation, functor and seed.

data CoalgA (a :: (* -> *) -> * -> *) (s :: *) (f :: * -> *) where
  Phi :: (s -> f s) -> CoalgA a s f

-- | Anamorphic coalgebra with annotation variable hidden as an existential.

type Coalg s f = forall a. CoalgA a s f

-- | Allow anamorphic algebras to be run as an apomorphism.

toApo :: Functor f => CoalgA a s f -> Apo.CoalgA a f s
toApo (Phi s) = Apo.Phi (fmap Left . s)

anaMA :: (Traversable f, AnnIO a f m) => CoalgA a s f -> s -> m (FixA a f)
anaMA = Apo.apoMA . toApo

anaM :: (Traversable f, Applicative m, Monad m) => CoalgA Id s f -> s -> m (Fix f)
anaM = Apo.apoM . toApo

anaA :: (Traversable f, AnnIO a f Identity) => CoalgA a s f -> s -> FixA a f
anaA = Apo.apoA . toApo

ana :: Traversable f => CoalgA Id s f -> s -> Fix f
ana = Apo.apo . toApo

