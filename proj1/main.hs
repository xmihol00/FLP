import System.Environment
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy.Read (decimal, double)

--module Main (main) where
--import Tree (BinaryTree(Node, Leaf))

data BinaryTree = Node { key :: Int, threshold :: Double, left :: BinaryTree, right :: BinaryTree} | Leaf { value :: String } 
    deriving (Show)

instance Read BinaryTree where
    readsPrec _ input = [readTree input 0]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False

takeRestOfText :: String -> (String, String)
takeRestOfText = span (\x -> x /= '\n' && x /= ' ' && x /= '\t')

skipWhiteSpaces :: String -> String
skipWhiteSpaces = dropWhile (\x -> x == ' ' || x == '\t')

takeErrorSample :: String -> String
takeErrorSample = takeWhile (/= '\n') . take 16

myError line text = error ("\nLine " ++ (show $ line + 1) ++ ": " ++ text ++ "\n")

readTree :: String -> Int -> (BinaryTree, String)
readTree ('N':'o':'d':'e':':':values) line = (Node parsedIndex parsedThreshold leftNode rightNode, restRight)
    where trimmed = skipWhiteSpaces values
          (parsedIndex, restIndex) = decimalOrError trimmed line
          (x:restSpaces) = skipWhiteSpaces restIndex
          check = (x == ',') || myError line ("Comma expected at: '" ++ (takeErrorSample (x:restSpaces)) ++ "...'")
          (parsedThreshold, restThreshold) = if check then doubleOrError restSpaces line else (0, "")
          ('\n':nextLineLeft) = skipWhiteSpaces restThreshold
          nextLineNum = line + 1
          removedIndentLeft = checkRemoveIndent nextLineLeft nextLineNum
          (leftNode, restLeft) = readTree removedIndentLeft nextLineNum
          ('\n':nextLineRight) = skipWhiteSpaces restLeft
          removedIndentRight = checkRemoveIndent nextLineRight nextLineNum
          (rightNode, restRight) = readTree removedIndentRight nextLineNum

readTree ('L':'e':'a':'f':':':values) line = (Leaf className, rest)
    where trimmed = skipWhiteSpaces values
          (className, rest) = takeRestOfText trimmed
readTree wrong line = myError line ("Unexpected key word at: '" ++ (takeErrorSample wrong) ++ "...'")

checkRemoveIndent :: String -> Int -> String
checkRemoveIndent input line = trimmed
    where (spaces, trimmedUnchecked) = splitAt (2 * line) input
          check = (all (==' ') spaces) || myError line ("Wrong indent at line " ++ (show line) ++ ", unexpected indent.")
          trimmed = if check then trimmedUnchecked else ""

decimalOrError :: String -> Int -> (Int, String)
decimalOrError input line = (num, rest)
    where numMaybe = decimal $ pack $ skipWhiteSpaces input
          (Right (num, restPacked)) = if isRight numMaybe then numMaybe else (myError line  ("Unable to parse '" ++ (takeErrorSample input) ++ "...' as a natural number."))
          rest = unpack restPacked

doubleOrError :: String -> Int -> (Double, String)
doubleOrError input line = (num, rest)
    where numMaybe = double $ pack $ skipWhiteSpaces input
          (Right (num, restPacked)) = if isRight numMaybe then numMaybe else (myError line  ("Unable to parse '" ++ (takeErrorSample input) ++ "...' as a floating point number."))
          rest = unpack restPacked

traverseTree :: BinaryTree -> [Double] -> String
traverseTree tree thresholds = error "TODO"

--parseArgs args
--    | length args == 2 = putStrLn "2 args"
--    | length args == 3 = putStrLn "3 args"

main :: IO ()
main = do
    input <- readFile "trees/example1.txt"
    let [(tree, _)] = reads input :: [(BinaryTree, String)]
    print tree
