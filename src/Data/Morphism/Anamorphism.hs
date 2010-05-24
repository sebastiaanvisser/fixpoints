{-# LANGUAGE
    GADTs
  , FlexibleContexts
  , RankNTypes
  , KindSignatures
  #-}
module Data.Morphism.Anamorphism where

import Data.Annotation
import Control.Monad.Identity
import Data.Traversable
import Data.Identity
import Data.Fixpoint
import qualified Data.Morphism.Apomorphism as Apo

data CoalgA (a :: (* -> *) -> * -> *) (s :: *) (f :: * -> *) where
  Phi :: (s -> f s) -> CoalgA a s f

type Coalg s f = forall a. CoalgA a s f

phi :: Functor f => CoalgA a s f -> Apo.CoalgA a f s
phi (Phi s) = Apo.Phi (fmap Left . s)

anaMA :: (Monad m, Traversable f, In a f m) => CoalgA a s f -> s -> m (FixA a f)
anaMA = Apo.apoMA . phi

anaA :: (Traversable f, In a f Identity) => CoalgA a s f -> s -> FixA a f
anaA = Apo.apoA . phi

ana :: Traversable f => CoalgA Id s f -> s -> Fix f
ana = Apo.apo . phi

