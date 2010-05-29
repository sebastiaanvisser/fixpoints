{-# LANGUAGE ScopedTypeVariables #-}
module Test where

import Data.List.Abstract
import Data.Tree.Abstract
import qualified Data.List.Pure as L
import qualified Data.Tree.Pure as T
import System.Random

main :: IO ()
main =
  do let ls = L.fromList [1..10] :: List Integer 
     print ("list:",    ls)
     print ("length:",  L.length ls :: Integer)
     print ("head:",    L.head ls)
     print ("last:",    L.last ls)
     print ("sum:",     L.sum ls)
     print ("product:", L.product ls)
     print ("prepend:", L.prepend [9,6,3,0] ls)
     print ("append:",  L.append [9,6,3,0] ls)

     let tr = T.fromList (map (\a -> (a,a*a)) [5, 3, 1, 0, 10, 23, 3, 6, 12, 3]) :: Tree Int Int
     print ("tree:",          tr)
     print ("size:",          T.size tr :: Integer)
     print ("depth:",         T.depth tr :: Integer)
     print ("lookup 10:",     T.lookup 10 tr)
     print ("lookup 3:",      T.lookup  3 tr)
     print ("lookup 14:",     T.lookup 14 tr)
     print ("lookupAll 10:",  T.lookupAll 10 tr)
     print ("lookupAll 3:",   T.lookupAll  3 tr)
     print ("lookupAll 14:",  T.lookupAll 14 tr)
     print ("head:",          T.head tr)
     print ("last:",          T.last tr)
     print ("minimum:",       T.minimum tr)
     print ("maximum:",       T.maximum tr)

     xs <- mapM (const randomIO) [1..20::Int]
     let nums = map (\a -> let b = mod a 100 in (b,b*b)) xs

     let tr1 = foldr (\(a,b) -> T.insert a b) T.empty nums :: Tree Int Int
     print ("insert 4,16:",   tr1)
     print ("asList:",        T.asList tr1)
     print ("asListR:",       T.asListR tr1)
     print "pretty:"
     putStrLn (T.prettyPrint tr1)

