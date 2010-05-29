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
import Data.Constant
import Data.Sum
import Data.Fixpoint
import Data.Identity
import Data.Traversable
import Prelude hiding (mapM)

data CoalgebraA (a :: (* -> *) -> * -> *) (f :: * -> *) where
  Phi :: (f (FixA a f) -> f (Either (FixA a f) (FixBotA a f))) -> CoalgebraA a f

type Coalgebra f = forall a. CoalgebraA a f

make :: f (FixBotA a f) -> Either (FixA a f) (FixBotA a f)
make = Right . In . L

next :: FixA a f -> Either (FixA a f) (FixBotA a f)
next = Left

stop :: FixA a f -> Either (FixA a f) (FixBotA a f)
stop = Right . In . R . K

endoapomorphismMA :: (Monad m, In a f m, Traversable f, OutIn a f m) => CoalgebraA a f -> FixA a f -> m (FixA a f)
endoapomorphismMA (Phi phi) = outIn1 (mapM (endoapomorphismMA (Phi phi) `either` topIn) . phi)

endoapomorphismA :: (OutIn a f Identity, Traversable f) => CoalgebraA a f -> FixA a f -> FixA a f
endoapomorphismA phi = runIdentity . endoapomorphismMA phi

endoapomorphism :: Traversable f => CoalgebraA Id f -> Fix f -> Fix f
endoapomorphism phi = fullyOutId . runIdentity . endoapomorphismMA phi . fullyInId

