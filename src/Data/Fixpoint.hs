{-# LANGUAGE
    FlexibleContexts
  , UndecidableInstances
  #-}
module Data.Fixpoint where

newtype Fix f = In { out :: f (Fix f) }

instance Show (f (Fix f)) => Show (Fix f) where
  show (In f) = "{" ++ show f ++ "}"

