{-# LANGUAGE TypeOperators #-}
module Data.Compose where

infixl 9 :.:

newtype (f :.: g) ix = C (f (g ix))

