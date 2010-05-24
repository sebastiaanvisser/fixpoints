{-# LANGUAGE
    RankNTypes
  , GADTs
  , TupleSections
  , FlexibleContexts
  , ScopedTypeVariables
  , KindSignatures
  #-}
module Data.Morphism.Endoparamorphism where

import Control.Monad hiding (mapM)
import Control.Monad.Identity
import Data.Annotation
import Data.Fixpoint
import Data.Identity
import Data.Traversable
import Prelude hiding (mapM)

data AlgA (a :: (* -> *) -> * -> *) (f :: * -> *) where
  Psi :: (f (FixA a f, FixA a f) -> FixBotA a f) -> AlgA a f

type Alg f = forall a. AlgA a f

endoMA :: (Monad m, Traversable f, InOut a f m) => AlgA a f -> FixA a f -> m (FixA a f)
endoMA (Psi psi) = topIn . psi <=< mapM (grp (endoMA (Psi psi))) <=< outA . out
  where grp f c = liftM (c,) (f c)

endoA :: (Traversable f, InOut a f Identity) => AlgA a f -> FixA a f -> FixA a f
endoA psi = runIdentity . endoMA psi

endo :: Traversable f => AlgA Id f -> Fix f -> FixA Id f
endo psi = runIdentity . endoMA psi . fullyInId

