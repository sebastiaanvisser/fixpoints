{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
module Data.List.Algebras where

-- import Data.Annotation
-- import qualified Data.Morphism.Apomorphism      as Apo
-- import qualified Data.Morphism.Paramorphism     as Para
import Data.Constant
import Data.Fixpoint
import Data.List.Abstract
import Data.Monoid
import Data.Sum
import Prelude hiding (tail, sum, product)
import qualified Data.Morphism.Anamorphism      as Ana
import qualified Data.Morphism.Catamorphism     as Cata
import qualified Data.Morphism.Endoparamorphism as Endopara
-- import qualified Data.Morphism.Endoapomorphism  as Endoapo

fromList :: Ana.Coalgebra [x] (ListF x)
fromList = Ana.Phi $ \f -> case f of [] -> Nil ; x:xs -> Cons x xs

foldMap :: Monoid m => (x -> m) -> Cata.Algebra (ListF x) m
foldMap f = Cata.Psi (list mempty (mappend . f))

fold :: Monoid m => Cata.Algebra (ListF m) m
fold = foldMap id

singleton :: Ana.Coalgebra (Maybe x) (ListF x)
singleton = Ana.Phi (maybe Nil (flip Cons Nothing))

length :: Num n => Cata.Algebra (ListF x) n
length = Cata.Psi (list 0 (const (1+)))

-- append :: [x] -> Endoapo.Coalgebra (ListF x)
-- append vs = Endoapo.Phi $ \f ->
--   case f of
--     Nil       -> undefined -- foldr (\x xs -> In $ Cons x xs) nil vs
--     Cons x xs -> Cons x (Left xs)


prepend :: [x] -> Endopara.Algebra (ListF x)
prepend vs = Endopara.Psi $ \f ->
  foldr (\x -> In . L . Cons x)
        (In (L (fmap (In . R . K . fst) f)))
        vs

cons :: x -> Endopara.Algebra (ListF x)
cons x = prepend [x]

sum :: Num x => Cata.Algebra (ListF x) (Sum x)
sum = foldMap Sum

product :: Num x => Cata.Algebra (ListF x) (Product x)
product = foldMap Product

last :: Cata.Algebra (ListF x) (Last x)
last = foldMap (Last . Just)

head :: Cata.Algebra (ListF x) (First x)
head = foldMap (First . Just)

lookup :: (Eq k, Monoid v) => k -> Cata.Algebra (ListF (k, v)) (Maybe v)
lookup k = foldMap (\(m, v) -> if k == m then Just v else Nothing)


