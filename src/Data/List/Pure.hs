module Data.List.Pure where

import Data.Monoid
import Prelude
-- import Data.Fixpoint
import Data.List.Abstract
import qualified Data.List.Algebras as Alg
import qualified Data.Morphism.Anamorphism      as Ana
import qualified Data.Morphism.Catamorphism     as Cata
import qualified Data.Morphism.Endoparamorphism as Endopara

fromList :: [x] -> List x
fromList = Ana.anamorphism Alg.fromList

length :: Num r => List x -> r
length = Cata.catamorphism Alg.length

head :: List x -> Maybe x
head = getFirst . Cata.catamorphism Alg.head

last :: List x -> Maybe x
last = getLast . Cata.catamorphism Alg.last

sum :: Num x => List x -> x
sum = getSum . Cata.catamorphism Alg.sum

product :: Num x => List x -> x
product = getProduct . Cata.catamorphism Alg.product

prepend :: [x] -> List x -> List x
prepend xs = Endopara.endoparamorphism (Alg.prepend xs)

