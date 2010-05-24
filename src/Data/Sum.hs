{-# LANGUAGE TypeOperators #-}
module Data.Sum where

infixl 6 :+:

data (f :+: g) a = L { hleft :: f a } | R { hright :: g a }

heither :: (l a -> b) -> (r a -> b) -> (l :+: r) a -> b
heither f _ (L l) = f l
heither _ g (R r) = g r

