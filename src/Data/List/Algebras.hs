module Data.List.Algebras where

import Data.Annotation
import Data.Fixpoint
import Data.List.Abstract
import qualified Data.List.Abstract as Ab
import Data.Monoid
import Data.Sum
import Prelude hiding (tail, sum, product)
import qualified Data.Morphism.Anamorphism      as Ana
import qualified Data.Morphism.Catamorphism     as Cata
import qualified Data.Morphism.Endoparamorphism as Endopara
import qualified Data.Morphism.Endoapomorphism  as Endoapo

fromList :: Ana.Coalgebra [x] (ListF x)
fromList = Ana.Phi $ \f -> case f of [] -> Nil ; x:xs -> Cons x xs

singleton :: Ana.Coalgebra (Maybe x) (ListF x)
singleton = Ana.Phi (maybe Nil (flip Cons Nothing))

----

length :: Num n => Cata.Algebra (ListF x) n
length = Cata.Psi (list 0 (const (1+)))

foldMap :: Monoid m => (x -> m) -> Cata.Algebra (ListF x) m
foldMap f = Cata.Psi (list mempty (mappend . f))

fold :: Monoid m => Cata.Algebra (ListF m) m
fold = foldMap id

----

head :: Cata.Algebra (ListF x) (First x)
head = foldMap (First . Just)

last :: Cata.Algebra (ListF x) (Last x)
last = foldMap (Last . Just)

sum :: Num x => Cata.Algebra (ListF x) (Sum x)
sum = foldMap Sum

product :: Num x => Cata.Algebra (ListF x) (Product x)
product = foldMap Product

lookup :: (Eq k, Monoid v) => k -> Cata.Algebra (ListF (k, v)) (Maybe v)
lookup k = foldMap (\(m, v) -> if k == m then Just v else Nothing)

----

prepend :: [x] -> Endopara.Algebra (ListF x)
prepend vs = Endopara.Psi $ \f ->
  foldr (\x -> In . L . Cons x)
        (In . L $ fmap (botA . fst) f)
        vs

cons :: x -> Endopara.Algebra (ListF x)
cons x = prepend [x]

append :: [x] -> Endoapo.Coalgebra (ListF x)
append []     = Endoapo.Phi (fmap Left)
append (v:vs) = Endoapo.Phi $ \f ->
  case f of
    Nil       -> Cons v (Right (bot (foldr Ab.cons Ab.nil vs)))
    Cons x xs -> Cons x (Left xs)

