module Tree () where

{-data BinaryTree a b c = Node { key :: a, threshold :: b, left :: BinaryTree a b c, right :: BinaryTree a b c} | Leaf { value :: c } 
    deriving (Show)
instance (Read a, Read b, Read c) => Read (BinaryTree a b c) where
    readsPrec _ input = [(Leaf (reads input :: c), input)]

data Point = Point Int Int deriving (Show)

instance Read Point where
    readsPrec _ input =
        let [(x, rest1)] = reads input
            [(y, rest2)] = reads rest1
        in [(Point x y, rest2)]-}