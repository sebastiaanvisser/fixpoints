{-# LANGUAGE GADTs, FlexibleContexts, RankNTypes, KindSignatures #-}
module Data.Morphism.Catamorphism where

import Control.Monad hiding (mapM)
import Control.Monad.Identity
import Data.Annotation
import Data.Fixpoint
import Data.Identity
import Data.Traversable
import Prelude hiding (mapM)
import qualified Data.Morphism.Paramorphism as Para

data AlgebraA (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
  Psi :: (f r -> r) -> AlgebraA a f r

type Algebra f r = forall a. AlgebraA a f r

psi :: Functor f => AlgebraA a f r -> Para.AlgebraA a f r
psi (Psi c) = Para.Psi (c . fmap snd)

catamorphismMA :: (Monad m, Traversable f, Out a f m) => AlgebraA a f r -> FixA a f -> m r
catamorphismMA = Para.paramorphismMA . psi

catamorphismA :: (Traversable f, Out a f Identity) => AlgebraA a f r -> FixA a f -> r
catamorphismA = Para.paramorphismA . psi

catamorphism :: Traversable f => AlgebraA Id f r -> Fix f -> r
catamorphism = Para.paramorphism . psi

