{-# LANGUAGE TypeOperators #-}
module Data.Product where

infixl 7 :*:

data (f :*: g) a = (:*:) { hfst :: f a, hsnd :: g a }

swap :: (f :*: g) ix -> (g :*: f) ix
swap (f :*: g) = g :*: f

