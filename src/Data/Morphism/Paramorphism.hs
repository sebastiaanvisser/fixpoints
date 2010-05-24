{-# LANGUAGE
    GADTs
  , FlexibleContexts
  , RankNTypes
  , KindSignatures
  , TupleSections
  #-}
module Data.Morphism.Paramorphism
( AlgebraA (Psi)
, Algebra
, paramorphismMA
, paramorphismA
, paramorphism
)
where

import Control.Applicative
import Control.Monad hiding (mapM)
import Control.Monad.Identity
import Data.Annotation
import Data.Fixpoint
import Data.Identity
import Data.Traversable
import Data.Tuples
import Prelude hiding (mapM)

data AlgebraA (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
  Psi  :: (f (FixA a f, r) -> r)  -> AlgebraA a f r
  Proj :: AlgebraA a f (r -> s, r, s) -> AlgebraA a f s

type Algebra f r = forall a. AlgebraA a f r

instance Functor f => Functor (AlgebraA a f) where
  fmap f psi = Proj (group (pure f) psi)

instance Functor f => Applicative (AlgebraA a f) where
  pure    = Psi . const
  a <*> b = Proj (group a b)

group :: (Functor f, Functor (AlgebraA a f))
      => AlgebraA a f (r -> s)
      -> AlgebraA a f r
      -> AlgebraA a f (r -> s, r, s)
group (Proj f) (Proj g) = group (fmap trd3 f) (fmap trd3 g)
group (Psi  f) (Proj g) = group (Proj (group (pure id) (Psi f))) (Proj g)
group (Proj f) (Psi  g) = group (Proj f) (Proj (group (pure id) (Psi g)))
group (Psi  f) (Psi  g) = Psi (\x -> f (ffmap fst3 x) `grp` g (ffmap snd3 x))
  where grp x y = (x, y, x y)
        ffmap = fmap . fmap

paramorphismMA :: (Monad m, Traversable f, Out a f m) => AlgebraA a f r -> FixA a f -> m r
paramorphismMA (Proj psi) = liftM trd3 . paramorphismMA psi
paramorphismMA (Psi  psi) = return . psi <=< mapM (grp (paramorphismMA (Psi psi))) <=< outA . out
  where grp f c = liftM (c,) (f c)

paramorphismA :: (Traversable f, Out a f Identity) => AlgebraA a f r -> FixA a f -> r
paramorphismA psi = runIdentity . paramorphismMA psi

paramorphism :: Traversable f => AlgebraA Id f r -> Fix f -> r
paramorphism p = runIdentity . paramorphismMA p . fullyInId

