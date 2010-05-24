{-# LANGUAGE FlexibleInstances, FlexibleContexts, MultiParamTypeClasses #-}
module Data.Debug where

import Control.Monad
import Data.Annotation

newtype Debug f ix = Debug { unDebug :: f ix }

instance (Functor f, Show (f ())) => Out   Debug f IO where outA   = printOut . unDebug
instance (Functor f, Show (f ())) => In    Debug f IO where inA    = return . Debug <=< printIn
instance (Functor f, Show (f ())) => OutIn Debug f IO where outInA = defaultOutIn

printOut :: (Functor f, Show (f ())) => f a -> IO (f a)
printOut f = print ("Out: " ++ show (unit f)) >> return f

printIn :: (Functor f, Show (f ())) => f a -> IO (f a)
printIn f = print ("In: " ++ show (unit f)) >> return f

unit :: Functor f => f a -> f ()
unit = fmap (const ())

