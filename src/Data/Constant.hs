module Data.Constant where

newtype K f ix = K { unK :: f }

castK :: K f a -> K f b
castK (K f) = K f

