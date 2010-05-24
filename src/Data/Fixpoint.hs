module Data.Fixpoint where

newtype Fix f = In { out :: f (Fix f) }

