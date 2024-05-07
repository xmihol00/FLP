data Tree k v = Node (Tree k v ) k v (Tree k v) | Empty
    deriving (Show)

tree = ((Node (Node Empty 1 'a' Empty) 2 'b' (Node Empty 3 'b' Empty)))
