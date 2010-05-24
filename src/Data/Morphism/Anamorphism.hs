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

data CoalgebraA (a :: (* -> *) -> * -> *) (s :: *) (f :: * -> *) where
  Phi :: (s -> f s) -> CoalgebraA a s f

type Coalgebra s f = forall a. CoalgebraA a s f

phi :: Functor f => CoalgebraA a s f -> Apo.CoalgebraA a f s
phi (Phi s) = Apo.Phi (fmap Left . s)

anamorphismMA :: (Monad m, Traversable f, In a f m) => CoalgebraA a s f -> s -> m (FixA a f)
anamorphismMA = Apo.apomorphismMA . phi

anamorphismA :: (Traversable f, In a f Identity) => CoalgebraA a s f -> s -> FixA a f
anamorphismA = Apo.apomorphismA . phi

anamorphism :: Traversable f => CoalgebraA Id s f -> s -> Fix f
anamorphism = Apo.apomorphism . phi

