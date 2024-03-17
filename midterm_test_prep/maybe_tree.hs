
data Tree k v = Empty | Node k v (Tree k v) (Tree k v)

instance (Show k, Show v) => Show (Tree k v) where
    show t = showTree t 0

showTree :: (Show k, Show v) => Tree k v -> Int -> String
showTree (Node k v l r) indent = (take indent [' ', ' ' ..]) ++ "Node: " ++ show k ++ " " ++ show v ++ "\n" ++ (showTree l (indent + 2)) ++ (showTree r (indent + 2))
showTree Empty _ = ""

tree :: Tree Int [Int]
tree = Node 3 [1, 2, 3] (Node 1 [3, 4] Empty (Node 2 [8, 7, 6, 5] Empty Empty)) (Node 6 [55, 66, 77, 88, 99] Empty Empty)

maybeInsert :: (Ord k) => Tree k v -> k -> v -> Maybe (Tree k v)
maybeInsert Empty k v = Just (Node k v Empty Empty)
maybeInsert (Node key value l r) k v
    | k == key = Nothing
    | k < key = maybeInsert l k v >>= (\x -> Just (Node key value x r))
    | otherwise = maybeInsert r k v >>= (\x -> Just (Node key value l x))

maybeFind :: (Ord k) => Tree k v -> k -> Maybe v
maybeFind Empty _ = Nothing
maybeFind (Node key value l r) k
    | k == key = Just value
    | k < key = maybeFind l k
    | otherwise = maybeFind r k

