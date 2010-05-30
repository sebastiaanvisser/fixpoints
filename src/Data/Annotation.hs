{-# LANGUAGE TypeOperators, MultiParamTypeClasses #-}
module Data.Annotation where

import Control.Monad hiding (mapM)
import Data.Constant
import Data.Fixpoint
import Data.Sum
import Data.Traversable
import Prelude hiding (mapM)

type FixA a f = Fix (a f)

class Out a f m where outA :: a f (FixA a f) -> m (  f (FixA a f)) 
class In  a f m where inA  ::   f (FixA a f) -> m (a f (FixA a f))

out1 :: (Monad m, Out a f m) => FixA a f -> m (f (FixA a f))
out1 = outA . out

in1 :: (Monad m, In a f m) => f (FixA a f) -> m (FixA a f)
in1 = return . In <=< inA

fullyOut :: (Monad m, Traversable f, Out a f m) => FixA a f -> m (Fix f)
fullyOut = return . In <=< mapM fullyOut <=< outA . out

fullyIn :: (Monad m, Traversable f, In a f m) => Fix f -> m (FixA a f)
fullyIn = return . In <=< inA <=< mapM fullyIn . out

type FixBotA a f = Fix (f :+: K (FixA a f))

bot :: Functor f => Fix f -> FixBotA a f
bot = In . L . fmap bot . out

botA :: FixA a f -> FixBotA a f
botA = In . R . K

topIn :: (Monad m, Traversable f, In a f m) => FixBotA a f -> m (FixA a f)
topIn = heither (return . In <=< inA <=< mapM topIn) (return . unK) . out

class (In a f m, Out a f m) => OutIn a f m where
  outInA ::  (f (FixA a f) -> m   (f (FixA a f)))
         -> a f (FixA a f) -> m (a f (FixA a f))

defaultOutIn
  :: (OutIn a f m, Monad m)
  =>  (f (FixA a f) -> m (  f (FixA a f)))
  -> a f (FixA a f) -> m (a f (FixA a f))
defaultOutIn f = inA <=< f <=< outA

outIn1 :: (Monad m, OutIn a f m) => (f (FixA a f) -> m (f (FixA a f))) -> FixA a f -> m (FixA a f)
outIn1 f = return . In <=< outInA f . out

