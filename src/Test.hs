{-# LANGUAGE ScopedTypeVariables #-}
module Test where

import Data.List.Abstract
import qualified Data.List.Pure as Pure

main :: IO ()
main =
  do let ls = Pure.fromList [1..10] :: List Int 
     print ("list:",    ls)
     print ("length:",  Pure.length ls :: Int)
     print ("head:",    Pure.head ls)
     print ("last:",    Pure.last ls)
     print ("sum:",     Pure.sum ls)
     print ("product:", Pure.product ls)
     print ("prepend:", Pure.prepend [9,6,3,0] ls)

