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

data AlgA (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
  Psi :: (f r -> r) -> AlgA a f r

type Alg f r = forall a. AlgA a f r

-- | Convert a catamorphic algebra to a paramorphic algebra.

psi :: Functor f => AlgA a f r -> Para.AlgA a f r
psi (Psi c) = Para.Psi (c . fmap snd)

cataMA :: (Monad m, Traversable f, Out a f m) => AlgA a f r -> FixA a f -> m r
cataMA = Para.paraMA . psi

cataA :: (Traversable f, Out a f Identity) => AlgA a f r -> FixA a f -> r
cataA = Para.paraA . psi

cata :: Traversable f => AlgA Id f r -> Fix f -> r
cata = Para.para . psi

