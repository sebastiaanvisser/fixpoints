module Data.Tree.Algebras where

-- import Data.Annotation
-- import Data.Fixpoint
import Data.Tree.Abstract
-- import qualified Data.Tree.Abstract as Ab
import Data.Monoid
-- import Data.Sum
-- import Prelude hiding (tail, sum, product)
import qualified Data.Morphism.Anamorphism      as Ana
import qualified Data.Morphism.Catamorphism     as Cata
-- import qualified Data.Morphism.Endoparamorphism as Endopara
-- import qualified Data.Morphism.Endoapomorphism  as Endoapo

fromSortedList :: Ana.Coalgebra [(k, v)] (TreeF k v)
fromSortedList = Ana.Phi $ \f ->
  case f of
    [] -> Leaf
    xs ->
      let l       = take (length xs `div` 2) xs
          (k,v):r = drop (length l         ) xs
      in Branch k v l r

lookup :: Ord k => k -> Cata.Algebra (TreeF k v) (Maybe v)
lookup k = Cata.Psi $ \f ->
  case f of
    Leaf           -> Nothing
    Branch c w l r ->
      case k `compare` c of
        LT -> l
        EQ -> Just w
        GT -> r

lookupAll :: Ord k => k -> Cata.Algebra (TreeF k v) [v]
lookupAll k = Cata.Psi $ \f ->
  case f of
    Leaf           -> []
    Branch c w l r ->
      case k `compare` c of
        LT -> l
        EQ -> w:(l ++ r)
        GT -> r

foldMap :: Monoid m => (v -> m) -> Cata.Algebra (TreeF k v) m
foldMap m = Cata.Psi $ \f ->
  case f of
    Leaf           -> mempty
    Branch _ v l r -> l `mappend` m v `mappend` r

foldMapR :: Monoid m => (v -> m) -> Cata.Algebra (TreeF k v) m
foldMapR m = Cata.Psi $ \f ->
  case f of
    Leaf           -> mempty
    Branch _ v l r -> r `mappend` m v `mappend` l

depth :: (Num r, Ord r) => Cata.Algebra (TreeF k v) r
depth = Cata.Psi $ \f ->
  case f of
    Leaf           -> 0
    Branch _ _ l r -> 1 + max l r

{-
insert :: Ord k => k -> v -> Apo.Coendo (F.Tree k v)
insert k v = Apo.Phi $ \(InF s) ->
  case s of
    F.Leaf           -> F.Branch k v (Right (InF F.Leaf)) (Right (InF F.Leaf))
    F.Branch m w l r ->
      case k `compare` m of
        LT -> F.Branch m w (Right l) (Left  r)
        EQ -> F.Branch k v (Right l) (Right r)
        GT -> F.Branch m w (Left  l) (Right r)
-}

