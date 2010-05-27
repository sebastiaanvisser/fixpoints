module Data.List.Pure where

import Data.List.Abstract
import Data.Monoid
import Prelude
import qualified Data.List.Algebras as Alg
import qualified Data.Morphism.Anamorphism      as Ana
import qualified Data.Morphism.Catamorphism     as Cata
import qualified Data.Morphism.Endoapomorphism  as Endoapo
import qualified Data.Morphism.Endoparamorphism as Endopara

fromList :: [x] -> List x
fromList = Ana.anamorphism Alg.fromList

singleton :: Maybe x -> List x
singleton = Ana.anamorphism Alg.singleton

length :: Num r => List x -> r
length = Cata.catamorphism Alg.length

fold :: Monoid x => List x -> x
fold = Cata.catamorphism Alg.fold

foldMap :: Monoid r => (x -> r) -> List x -> r
foldMap f = Cata.catamorphism (Alg.foldMap f)

head :: List x -> Maybe x
head = getFirst . Cata.catamorphism Alg.head

last :: List x -> Maybe x
last = getLast . Cata.catamorphism Alg.last

sum :: Num x => List x -> x
sum = getSum . Cata.catamorphism Alg.sum

product :: Num x => List x -> x
product = getProduct . Cata.catamorphism Alg.product

lookup :: (Eq k, Monoid v) => k -> List (k, v) -> Maybe v
lookup k = Cata.catamorphism (Alg.lookup k)

append :: [x] -> List x -> List x
append xs = Endoapo.endoapomorphism (Alg.append xs)

cons :: x -> List x -> List x
cons x = Endopara.endoparamorphism (Alg.cons x)

prepend :: [x] -> List x -> List x
prepend xs = Endopara.endoparamorphism (Alg.prepend xs)

