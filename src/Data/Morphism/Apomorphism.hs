{-# LANGUAGE
    TypeOperators
  , GADTs
  , KindSignatures
  , FlexibleContexts
  , RankNTypes
  , ScopedTypeVariables
  #-}
module Data.Morphism.Apomorphism where

-- import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Identity
import Data.Annotation
import Data.Fixpoint
import Data.Identity
import Data.Traversable
import Prelude hiding (mapM)

data CoalgebraA (a :: (* -> *) -> * -> *) (f :: * -> *) (s :: *) where
  Phi :: (s -> f (Either s (FixBotA a f))) -> CoalgebraA a f s

type Coalgebra s f = forall a. CoalgebraA a f s

apomorphismMA :: (Monad m, Traversable f, In a f m) => CoalgebraA a f s -> s -> m (FixA a f)
apomorphismMA (Phi phi) = return . In <=< inA <=< mapM (apomorphismMA (Phi phi) `either` topIn) . phi

apomorphismA :: (Traversable f, In a f Identity) => CoalgebraA a f s -> s -> FixA a f
apomorphismA phi = runIdentity . apomorphismMA phi

apomorphism :: Traversable f => CoalgebraA Id f s -> s -> Fix f
apomorphism phi = fullyOutId . runIdentity . apomorphismMA phi

