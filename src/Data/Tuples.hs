{-# LANGUAGE TupleSections #-}
module Data.Tuples where

import Control.Applicative
import Data.Foldable
import Data.Traversable

-- 3-tuples

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z

instance Functor ((,,) a b) where
  fmap f (a, b, c) = (a, b, f c)

instance Foldable ((,,) a b) where
  foldMap f = f . trd3

instance Traversable ((,,) a b) where
  traverse f (a, b, c) = (a, b,) <$> f c

