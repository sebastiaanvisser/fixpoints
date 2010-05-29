{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Monoid.Bounds where

import Data.Monoid

newtype Minimum n = Minimum { getMinimum :: n } deriving (Eq, Ord, Bounded)
newtype Maximum n = Maximum { getMaximum :: n } deriving (Eq, Ord, Bounded)

instance (Ord n, Bounded n) => Monoid (Minimum n) where
  mempty  = maxBound
  mappend = min

instance (Ord n, Bounded n) => Monoid (Maximum n) where
  mempty  = minBound
  mappend = max

