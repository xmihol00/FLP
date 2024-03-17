
data Tree k v = EmptyTree | Node k v (Tree k v) (Tree k v)

instance (Ord k) => Functor (Tree k) where
  fmap f EmptyTree = EmptyTree
  fmap f (Node k v left right) = Node k (f v) (fmap f left) (fmap f right)

instance (Show k, Show v) => Show (Tree k v) where
    show t = showTree t 0

showTree :: (Show k, Show v) => Tree k v -> Int -> String
showTree (Node k v l r) indent = (take indent [' ', ' ' ..]) ++ "Node: " ++ show k ++ " " ++ show v ++ "\n" ++ (showTree l (indent + 2)) ++ (showTree r (indent + 2))
showTree EmptyTree _ = ""

tree :: Tree Int [Int]
tree = Node 2 [1, 2, 3] (Node 1 [3, 4] EmptyTree (Node 3 [8, 7, 6, 5] EmptyTree EmptyTree)) (Node 4 [55, 66, 77, 88, 99] EmptyTree EmptyTree)


addSumCheck :: (Ord a, Num a) => a -> a -> [a] -> Bool
addSumCheck add check = (>check) . sum . map (+add)

listOfJust = [Just 1, Just 2, Just 7]
addJust val = map ((<$>) (+val))
unpackFunctor :: (Applicative f) => [f a] -> f [a]
unpackFunctor = foldr ((<*>) . ((<$>) (:))) (pure [])

--fmap length tree
--fmap sum tree
--fmap ((>10) . sum) tree
--fmap (>10) $ fmap sum $ tree