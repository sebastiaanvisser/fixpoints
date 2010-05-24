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

data CoalgA (a :: (* -> *) -> * -> *) (f :: * -> *) (s :: *) where
  Phi :: (s -> f (Either s (FixBotA a f))) -> CoalgA a f s

type Coalg s f = forall a. CoalgA a f s

apoMA :: (Monad m, Traversable f, In a f m) => CoalgA a f s -> s -> m (FixA a f)
apoMA (Phi phi) = return . In <=< inA <=< mapM (apoMA (Phi phi) `either` topIn) . phi

apoA :: (Traversable f, In a f Identity) => CoalgA a f s -> s -> FixA a f
apoA phi = runIdentity . apoMA phi

apo :: Traversable f => CoalgA Id f s -> s -> Fix f
apo phi = fullyOutId . runIdentity . apoMA phi

