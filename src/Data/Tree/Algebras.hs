module Data.Tree.Algebras where

import Data.Tree.Abstract
import Data.Monoid
import Data.Maybe
import qualified Data.Morphism.Anamorphism      as Ana
import qualified Data.Morphism.Catamorphism     as Cata
-- import qualified Data.Morphism.Endoparamorphism as Endopara
import qualified Data.Morphism.Endoapomorphism  as Endoapo
import Data.Morphism.Endoapomorphism (make, stop, next)

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

-- balanced = Cata.Psi $ \f a ->
--   case f of
--     Leaf           -> False
--     Branch _ _ l r -> True


depth :: (Num r, Ord r) => Cata.Algebra (TreeF k v) r
depth = Cata.Psi $ \f ->
  case f of
    Leaf           -> 0
    Branch _ _ l r -> 1 + max l r

insert :: Ord k => k -> v -> Endoapo.Coalgebra (TreeF k v)
insert k v = Endoapo.Phi $ \s ->
  case s of
    Leaf           -> Branch k v (make Leaf) (make Leaf)
    Branch m w l r ->
      case k `compare` m of
        LT -> Branch m w (next l) (stop r)
        _  -> Branch m w (stop l) (next r)

prettyPrint :: (Show k, Show v) => Cata.Algebra (TreeF k v) (Int, Int, Maybe [String])
prettyPrint = Cata.Psi $ \f ->
  case f of
    Leaf -> (0, 0, Nothing)
    Branch k v (_, i, l) (j, _, r) ->
      let txt = concat ["(", show k, ", ", show v, ")"]
          g x = fromMaybe [] . fmap (map (replicate (length txt) ' '++) . x)
          a = g (linea i) l
          b = g (lineb j) r
      in (length a, length b, Just (a ++ [txt] ++ b))
  where
    lineb = line (flip (++)) (flip (++)) "\\"
    linea = line (++) (++) "/"
    line o u c w b =
      zipWith (++) (replicate (length b - w) (replicate w ' ')
               `u` map (\i -> replicate (w-i-1) ' ' `o` c
               `o` replicate i ' ') [0..w-1]) b

