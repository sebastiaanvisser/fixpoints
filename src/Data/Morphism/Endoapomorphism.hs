{-# LANGUAGE
    TypeOperators
  , GADTs
  , KindSignatures
  , FlexibleContexts
  , RankNTypes
  , ScopedTypeVariables
  #-}
module Data.Morphism.Endoapomorphism where

import Control.Monad hiding (mapM)
import Control.Monad.Identity
import Data.Annotation
import Data.Fixpoint
import Data.Identity
import Data.Traversable
import Prelude hiding (mapM)

data CoalgA (a :: (* -> *) -> * -> *) (f :: * -> *) where
  Phi :: (f (FixA a f) -> f (Either (FixA a f) (FixBotA a f))) -> CoalgA a f

type Coalg f = forall a. CoalgA a f

endoMA :: (Monad m, In a f m, Traversable f, OutIn a f m) => CoalgA a f -> FixA a f -> m (FixA a f)
endoMA (Phi phi) = outIn1 (mapM (endoMA (Phi phi) `either` topIn) . phi)

endoA :: (OutIn a f Identity, Traversable f) => CoalgA a f -> FixA a f -> FixA a f
endoA phi = runIdentity . endoMA phi

endo :: Traversable f => CoalgA Id f -> Fix f -> Fix f
endo phi = fullyOutId . runIdentity . endoMA phi . fullyInId

