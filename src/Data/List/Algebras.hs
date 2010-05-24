{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
module Data.List.Algebras where

-- import Data.Annotation
-- import Data.Monoid
-- import qualified Data.Morphism.Apomorphism      as Apo
-- import qualified Data.Morphism.Endoapomorphism  as Endoapo
-- import qualified Data.Morphism.Paramorphism     as Para
import Data.Constant
import Data.Fixpoint
import Data.List.Abstract
import Data.Sum
import Prelude hiding (tail)
import qualified Data.Morphism.Anamorphism      as Ana
import qualified Data.Morphism.Catamorphism     as Cata
import qualified Data.Morphism.Endoparamorphism as Endopara

fromList :: Ana.Coalgebra [x] (ListF x)
fromList = Ana.Phi $ \f -> case f of [] -> Nil ; x:xs -> Cons x xs

singleton :: Ana.Coalgebra (Maybe x) (ListF x)
singleton = Ana.Phi (maybe Nil (flip Cons Nothing))

length :: Num n => Cata.Algebra (ListF x) n
length = Cata.Psi (list 0 (const (1+)))

prepend :: [x] -> Endopara.Algebra (ListF x)
prepend vs = Endopara.Psi $ \f ->
  foldr (\x xs -> In (L (Cons x xs)))
        (In (L (fmap (In . R . K . fst) f)))
        vs

cons :: x -> Endopara.Algebra (ListF x)
cons x = prepend [x]

head :: Cata.AlgebraA a (ListF x) (Maybe x)
head = Cata.Psi (list Nothing (\x _ -> Just x))

-- last :: Cata.Alg (ListF x) (Last x)
-- last = Cata.Psi $ \f ->
--   case f of
--     Nil       -> Last Nothing
--     Cons x xs -> Last (Just x) `mappend` xs

-- ini :: Cata.AlgA a (ListF t) (ListF x f)
-- ini = Cata.Psi $ \f ->
--   case f of
--     Nil       -> Nothing
--     Cons x xs -> fmap 

-- test :: List x -> Maybe x
-- test = getLast . Cata.cata tail

