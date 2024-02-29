--import Data.Text.Lazy (Text)
import Data.Text.Lazy (pack)
import Data.Text.Lazy.Read (decimal, signed)

--module Main (main) where
--import Tree (BinaryTree(Node, Leaf))

data BinaryTree = Node { key :: Int, threshold :: Float, left :: BinaryTree, right :: BinaryTree} | Leaf { value :: String } 
    deriving (Show)

indentHelper :: String -> Int -> String
indentHelper input line =  if verified_spaces then branchHelper trimmed line else (error ("Wrong indent at line " ++ (show line) ++ ", unexpected indent."))
    where (spaces, trimmed) = splitAt (2 * line) input
          verified_spaces = all (==' ') spaces

branchHelper ('N':'o':'d':'e':':':rest) line = rest
branchHelper ('L':'e':'a':'f':':':rest) line = rest
branchHelper _ _ = error ("Unexpected key word at")

decimalOrError input = y
    where num = decimal $ pack input
          (Left x) = num
          (Right y) = num

leafHelper input line = Leaf input

--instance Read BinaryTree where
--    readsPrec _ input = indentHelper input 0

--main :: IO ()
--main = do
--    input <- readFile "trees/example1.txt"
--    print $ show input
