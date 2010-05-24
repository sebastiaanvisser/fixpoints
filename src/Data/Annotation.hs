{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}
module Data.Annotation where

import Control.Monad hiding (mapM)
import Data.Constant
import Data.Fixpoint
import Data.Sum
import Data.Traversable
import Prelude hiding (mapM)

type FixA a f = Fix (a f)

type FixTopA a f = Fix (a f :+: K (Fix    f)) 
type FixBotA a f = Fix (  f :+: K (FixA a f))

class In    a f m where inA  ::   f g -> m (a f g)
class Out   a f m where outA :: a f g -> m (  f g) 

topIn :: (Monad m, Traversable f, In a f m) => FixBotA a f -> m (FixA a f)
topIn = heither (return . In <=< inA <=< mapM topIn) (return . unK) . out

topOut :: (Monad m, Traversable f, Out a f m) => FixTopA a f -> m (Fix f)
topOut = heither (return . In <=< mapM topOut <=< outA) (return . unK) . out

fullyIn :: (Monad m, Traversable f, In a f m) => Fix f -> m (FixA a f)
fullyIn = return . In <=< inA <=< mapM fullyIn . out

fullyOut :: (Monad m, Traversable f, Out a f m) => FixA a f -> m (Fix f)
fullyOut = return . In <=< mapM fullyOut <=< outA . out

class (In a f m, Out a f m) => InOut a f m where
  inOutA :: (f g -> m (f g)) -> a f g -> m (a f g)

defaultInOut :: (InOut a f m, Monad m) => (f g -> m (f g)) -> a f g -> m (a f g)
defaultInOut f = inA <=< f <=< outA

