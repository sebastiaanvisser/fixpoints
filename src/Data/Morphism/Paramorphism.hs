{-# LANGUAGE
    GADTs
  , FlexibleContexts
  , RankNTypes
  , KindSignatures
  , TupleSections
  #-}
module Data.Morphism.Paramorphism
( AlgA (Psi)
, Alg
, paraMA
, paraA
, para
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

data AlgA (a :: (* -> *) -> * -> *) (f :: * -> *) (r :: *) where
  Psi  :: (f (FixA a f, r) -> r)  -> AlgA a f r
  Proj :: AlgA a f (r -> s, r, s) -> AlgA a f s

type Alg f r = forall a. AlgA a f r

instance Functor f => Functor (AlgA a f) where
  fmap f psi = Proj (group (pure f) psi)

instance Functor f => Applicative (AlgA a f) where
  pure    = Psi . const
  a <*> b = Proj (group a b)

group :: (Functor f, Functor (AlgA a f))
      => AlgA a f (r -> s)
      -> AlgA a f r
      -> AlgA a f (r -> s, r, s)
group (Proj f) (Proj g) = group (fmap trd3 f) (fmap trd3 g)
group (Psi  f) (Proj g) = group (Proj (group (pure id) (Psi f))) (Proj g)
group (Proj f) (Psi  g) = group (Proj f) (Proj (group (pure id) (Psi g)))
group (Psi  f) (Psi  g) = Psi (\x -> f (ffmap fst3 x) `grp` g (ffmap snd3 x))
  where grp x y = (x, y, x y)
        ffmap = fmap . fmap

-- | Paramorphism for annotations structures in a monadic context.

paraMA :: (Monad m, Traversable f, Out a f m) => AlgA a f r -> FixA a f -> m r
paraMA (Proj psi) = liftM trd3 . paraMA psi
paraMA (Psi  psi) = return . psi <=< mapM (grp (paraMA (Psi psi))) <=< outA . out
  where grp f c = liftM (c,) (f c)

-- | Paramorphism for annotated structures.

paraA :: (Traversable f, Out a f Identity) => AlgA a f r -> FixA a f -> r
paraA psi = runIdentity . paraMA psi

-- | Paramorphism.

para :: Traversable f => AlgA Id f r -> Fix f -> r
para p = runIdentity . paraMA p . fullyInId

