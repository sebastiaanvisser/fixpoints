{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}
module Data.Annotation where

import Control.Monad hiding (mapM)
import Data.Constant
import Data.Fixpoint
import Data.Sum
import Data.Traversable
import Prelude hiding (mapM)

type FixA a f = Fix (a f)

type FixBotA a f = Fix (  f :+: K (FixA a f))
type FixTopA a f = Fix (a f :+: K (Fix    f)) 

class Out   a f m where outA :: a f g -> m (  f g) 
class In    a f m where inA  ::   f g -> m (a f g)

out1 :: (Monad m, Out a f m) => FixA a f -> m (f (FixA a f))
out1 = outA . out

in1 :: (Monad m, In a f m) => f (FixA a f) -> m (FixA a f)
in1 = return . In <=< inA

topOut :: (Monad m, Traversable f, Out a f m) => FixTopA a f -> m (Fix f)
topOut = heither (return . In <=< mapM topOut <=< outA) (return . unK) . out

topIn :: (Monad m, Traversable f, In a f m) => FixBotA a f -> m (FixA a f)
topIn = heither (return . In <=< inA <=< mapM topIn) (return . unK) . out

fullyOut :: (Monad m, Traversable f, Out a f m) => FixA a f -> m (Fix f)
fullyOut = return . In <=< mapM fullyOut <=< outA . out

fullyIn :: (Monad m, Traversable f, In a f m) => Fix f -> m (FixA a f)
fullyIn = return . In <=< inA <=< mapM fullyIn . out

class (In a f m, Out a f m) => OutIn a f m where
  outInA :: (f g -> m (f g)) -> a f g -> m (a f g)

defaultOutIn :: (OutIn a f m, Monad m) => (f g -> m (f g)) -> a f g -> m (a f g)
defaultOutIn f = inA <=< f <=< outA

outIn1 :: (Monad m, OutIn a f m) => (f (FixA a f) -> m (f (FixA a f))) -> FixA a f -> m (FixA a f)
outIn1 f = return . In <=< outInA f . out

