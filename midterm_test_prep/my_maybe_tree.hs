
data Tree k v = Node k v (Tree k v) (Tree k v) | Leaf
instance (Show k, Show v) => Show (Tree k v) where
    show t = init (showIndented t 0)
showIndented Leaf _ = ""
showIndented (Node k v l r) i = take i [' ', ' ' ..] ++ "Node " ++ show k ++ ": " ++ show v ++ "\n" ++ showIndented l (i+2) ++ showIndented r (i+2)

tree1 = (Node 16 "sixteen" (Node 8 "eight" (Node 4 "four" Leaf Leaf) Leaf) (Node 32 "thirty two" Leaf (Node 64 "sixty four" Leaf Leaf)))

maybeReturn :: a -> Maybe a
maybeReturn = Just

maybeApply :: Maybe a -> (a -> b) -> Maybe b
maybeApply Nothing _ = Nothing
maybeApply (Just x) f = maybeReturn (f x)

maybeInsert :: (Ord k) => Tree k v -> k -> v -> Maybe (Tree k v)
maybeInsert (Node k v l r) key value
    | key == k = Nothing
    | key < k = maybeApply (maybeInsert l key value) (\x -> Node k v x r)
    | otherwise = maybeApply (maybeInsert r key value) (\x -> Node k v l x)
maybeInsert Leaf k v = maybeReturn (Node k v Leaf Leaf)
