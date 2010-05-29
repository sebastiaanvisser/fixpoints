module Data.Tree.Pure where

import Control.Applicative
import Data.List
import Data.Monoid
import Data.Monoid.Bounds
import Data.Maybe
import Data.Ord
import Data.Tuples
import Data.Tree.Abstract
import Prelude
import qualified Data.Morphism.Anamorphism      as Ana
import qualified Data.Morphism.Catamorphism     as Cata
import qualified Data.Morphism.Endoapomorphism  as Endoapo
-- import qualified Data.Morphism.Endoparamorphism as Endopara
import qualified Data.Tree.Algebras as Alg

fromList :: Ord k => [(k, v)] -> Tree k v
fromList = Ana.anamorphism Alg.fromSortedList . sortBy (comparing fst)

empty :: Ord k => Tree k v
empty = fromList []

lookup :: Ord k => k -> Tree k v -> Maybe v
lookup = Cata.catamorphism . Alg.lookup

lookupAll :: Ord k => k -> Tree k v -> [v]
lookupAll = Cata.catamorphism . Alg.lookupAll

insert :: Ord k => k -> v -> Tree k v -> Tree k v
insert k v = Endoapo.endoapomorphism (Alg.insert k v)

foldMap :: Monoid m => (v -> m) -> Tree k v -> m
foldMap = Cata.catamorphism . Alg.foldMap

foldMapR :: Monoid m => (v -> m) -> Tree k v -> m
foldMapR = Cata.catamorphism . Alg.foldMapR

fold :: Monoid v => Tree k v -> v
fold = foldMap id

foldR :: Monoid v => Tree k v -> v
foldR = foldMapR id

collect :: (Applicative f, Monoid (f v)) => Tree k v -> f v
collect = foldMap pure

collectR :: (Applicative f, Monoid (f v)) => Tree k v -> f v
collectR = foldMapR pure

asList :: Tree k v -> [v]
asList = collect

asListR :: Tree k v -> [v]
asListR = collectR

size :: Num n => Tree k v -> n
size = getSum . foldMap (const (Sum 1))

depth :: (Num r, Ord r) => Tree k v -> r
depth = Cata.catamorphism Alg.depth

head :: Tree k v -> Maybe v
head = getFirst . foldMap (First . Just)

last :: Tree k v -> Maybe v
last = getFirst . foldMapR (First . Just)

sum :: Num v => Tree k v -> v
sum = getSum . foldMap Sum

product :: Num v => Tree k v -> v
product = getProduct . foldMap Product

minimum :: (Ord v, Bounded v) => Tree k v -> v
minimum = getMinimum . foldMap Minimum

maximum :: (Ord v, Bounded v) => Tree k v -> v
maximum = getMaximum . foldMap Maximum

prettyPrint :: (Show k, Show v) => Tree k v -> String
prettyPrint = intercalate "\n" . fromMaybe [] . trd3 . Cata.catamorphism Alg.prettyPrint

