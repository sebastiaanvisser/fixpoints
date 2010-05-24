{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Data.Identity where

import Control.Monad.Identity
import Data.Annotation
import Data.Fixpoint
import Data.Traversable

newtype Id f ix = Id { unId :: f ix }

instance In    Id f Identity where inA    = return . Id
instance Out   Id f Identity where outA   = return . unId
instance InOut Id f Identity where inOutA = defaultInOut

type FixId f = FixA Id f

fullyInId :: Traversable f => Fix f -> FixId f
fullyInId = runIdentity . fullyIn

fullyOutId :: Traversable f => FixId f -> Fix f
fullyOutId = runIdentity . fullyOut

