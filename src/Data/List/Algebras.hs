module Data.List.Algebras where

-- import Generics.Fixpoint
-- import qualified Data.Morphism.Anamorphism   as Ana
-- import qualified Data.Morphism.Apomorphism   as Apo
-- import qualified Generics.Morphism.Para  as Para
import Data.List.Abstract
import Data.Monoid
import Prelude hiding (tail)
import qualified Data.Morphism.Catamorphism  as Cata

{-fromList :: Ana.Coalg [x] (ListF x)
fromList = Ana.Phi $ \f ->
  case f of
    []   -> Nil
    x:xs -> Cons x xs

singleton :: Ana.CoalgA a (Maybe x) (ListF x)
singleton = Ana.Phi $ \f ->
  case f of
    Nothing -> Nil
    Just x  -> Cons x Nothing-}

-- length :: Num n => Cata.Alg (ListF t) n
-- length = Cata.Psi $ \f ->
--   case f of
--     Nil      -> 0
--     Cons _ n -> 1 + n

{-cons :: x -> Apo.Coendo (ListF x)
cons v = Apo.Phi $ Cons v . Left-}

-- head :: Cata.Alg (ListF x) (Maybe x)
-- head = Cata.Psi $ \f ->
--   case f of
--     Nil      -> Nothing
--     Cons x _ -> Just x

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

