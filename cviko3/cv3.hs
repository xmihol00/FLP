class UsablePayload a where
    getKey   :: (Ord b) => a -> b
    eq       :: a -> a -> Bool
    lessThan :: a -> a -> Bool

data T a = E | N a (T a) (T a)
    deriving (Show)

t = N 4 (N 2 E E) (N 6 E E)

treeElem :: (UsablePayload a, Ord b) => b -> T a -> Bool
treeElem elem (N a x y)
    | elem == getKey a = True
    | elem < getKey a = treeElem elem x
    | otherwise = treeElem elem y
treeElem _ E = False

treeInsert :: (UsablePayload a) => a -> T a -> T a
treeInsert elem E = (N elem E E)
treeInsert elem t@(N a x y)
    | eq elem a = t
    | lessThan elem a = (N a (treeInsert elem x) y)
    | otherwise = (N a x (treeInsert elem y))

createTree :: (UsablePayload a) => [a] -> T a
createTree = foldl (\y x -> treeInsert x y) E

createTreeRev :: (UsablePayload a) => [a] -> T a
createTreeRev = foldr (\x y -> treeInsert x y) E

--data MyPayload a = Data a String
--
--instance (a, Ord b) => UsablePayload b MyPayload a where
--    getKey (Data key _) = key
--    eq (Data key1 _) (Data key2 _) = key1 == key2
--    lessThan (Data key1 _) (Data key2 _) = key1 < key2
--
--myGetKey :: (Ord a) => MyPayload a -> a
--myGetKey (Data key _) = key
