import System.IO

data Tree a = Node { key :: Int, value :: a, left :: Tree a, right :: Tree a} | Leaf { key :: Int, value :: a } deriving (Show)

example = Node 1 "a" (Node 0 "b" (Leaf 3 "c") (Leaf 4 "d")) (Node 2 "e" (Leaf 5 "f") (Leaf 6 "g"))

inOrder :: Show a => Tree a -> String
inOrder (Node  key value left right ) = (inOrder left) ++ show (key, value) ++ (inOrder right)
inOrder (Leaf key value) = show (key, value) 

preOrder :: Show a => Tree a -> String
preOrder (Node  key value left right ) = show (key, value) ++ ' ':(preOrder left) ++ ' ':(preOrder right)
preOrder (Leaf key value) = show (key, value) 

printInOrder :: Show a => Tree a -> IO ()
printInOrder x = putStrLn $ inOrder x

printTree :: IO ()
printTree = putStrLn $ show example