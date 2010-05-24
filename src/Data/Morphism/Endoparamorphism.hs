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

data AlgebraA (a :: (* -> *) -> * -> *) (f :: * -> *) where
  Psi :: (f (FixA a f, FixA a f) -> FixBotA a f) -> AlgebraA a f

type Algebra f = forall a. AlgebraA a f

endoparamorphismMA :: (Monad m, Traversable f, OutIn a f m) => AlgebraA a f -> FixA a f -> m (FixA a f)
endoparamorphismMA (Psi psi) = topIn . psi <=< mapM (grp (endoparamorphismMA (Psi psi))) <=< outA . out
  where grp f c = liftM (c,) (f c)

endoparamorphismA :: (Traversable f, OutIn a f Identity) => AlgebraA a f -> FixA a f -> FixA a f
endoparamorphismA psi = runIdentity . endoparamorphismMA psi

endoparamorphism :: Traversable f => AlgebraA Id f -> Fix f -> FixA Id f
endoparamorphism psi = runIdentity . endoparamorphismMA psi . fullyInId

