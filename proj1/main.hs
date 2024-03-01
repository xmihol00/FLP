{-# LANGUAGE RecordWildCards #-}

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Read as TL
import qualified Data.Text.IO as T
import qualified Data.Text as T

--module Main (main) where
--import Tree (BinaryTree(Node, Leaf))

data BinaryTree = Node { key :: Int, threshold :: Double, left :: BinaryTree, right :: BinaryTree} | Leaf { className :: String }

instance Read BinaryTree where
    readsPrec _ input = [readTree input 0]
instance Show BinaryTree where
    show tree = treeString
        where treeString = init $ showBinaryTree tree 0

showBinaryTree :: BinaryTree -> Int -> [Char]
showBinaryTree Node {..} depth = treeString
    where indent = take (2 * depth) [' ', ' ' ..]
          nextDepth = depth + 1
          leftRest = showBinaryTree left nextDepth
          rightRest = showBinaryTree right nextDepth
          treeString = indent ++ "Node: " ++ show key ++ ", " ++ show threshold ++ "\n" ++ leftRest ++ rightRest
showBinaryTree (Leaf className) depth = indent ++ "Leaf: " ++ className ++ "\n"
    where indent = take (2 * depth) [' ', ' ' ..]

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _)  = False

takeRestOfText :: String -> (String, String)
takeRestOfText = span (\x -> x /= '\n' && x /= ' ' && x /= '\t')

skipWhiteSpaces :: String -> String
skipWhiteSpaces = dropWhile (\x -> x == ' ' || x == '\t')

skipWhiteSpacesAndNewLines :: [Char] -> [Char]
skipWhiteSpacesAndNewLines = dropWhile (\x -> x == ' ' || x == '\t' || x == '\n')

takeErrorSample :: String -> String
takeErrorSample = takeWhile (/= '\n') . take 16

myError :: (Show a1, Num a1) => a1 -> [Char] -> a2
myError line text = error ("\nLine " ++ show (line + 1) ++ ": " ++ text ++ "\n")

readTree :: String -> Int -> (BinaryTree, String)
readTree ('N':'o':'d':'e':':':values) line = (Node parsedIndex parsedThreshold leftNode rightNode, restRight)
    where trimmed = skipWhiteSpaces values
          (parsedIndex, restIndex) = decimalOrError trimmed line
          (x:restSpaces) = skipWhiteSpaces restIndex
          check = (x == ',') || myError line ("Comma expected at: '" ++ takeErrorSample (x:restSpaces) ++ "...'")
          (parsedThreshold, restThreshold) = if check then doubleOrError restSpaces line else (0, "")
          ('\n':nextLineLeft) = skipWhiteSpaces restThreshold
          nextLineNum = line + 1
          removedIndentLeft = checkRemoveIndent nextLineLeft nextLineNum
          (leftNode, restLeft) = readTree removedIndentLeft nextLineNum
          ('\n':nextLineRight) = skipWhiteSpaces restLeft
          removedIndentRight = checkRemoveIndent nextLineRight nextLineNum
          (rightNode, restRight) = readTree removedIndentRight nextLineNum

readTree ('L':'e':'a':'f':':':values) _ = (Leaf className, rest)
    where trimmed = skipWhiteSpaces values
          (className, rest) = takeRestOfText trimmed
readTree wrong line = myError line $ "Unexpected key word at: '" ++ takeErrorSample wrong ++ "...'"

checkRemoveIndent :: String -> Int -> String
checkRemoveIndent input line = trimmed
    where (spaces, trimmedUnchecked) = splitAt (2 * line) input
          check = all (==' ') spaces || myError line ("Wrong indent at line " ++ show line ++ ", unexpected indent.")
          trimmed = if check then trimmedUnchecked else ""

decimalOrError :: String -> Int -> (Int, String)
decimalOrError input line = (num, rest)
    where numMaybe = TL.decimal $ TL.pack $ skipWhiteSpaces input
          (Right (num, restPacked)) = if isRight numMaybe then numMaybe else myError line $ "Unable to parse '" ++ takeErrorSample input ++ "...' as a natural number."
          rest = TL.unpack restPacked

doubleOrError :: String -> Int -> (Double, String)
doubleOrError input line = (num, rest)
    where numMaybe = TL.double $ TL.pack $ skipWhiteSpaces input
          (Right (num, restPacked)) = if isRight numMaybe then numMaybe else myError line $ "Unable to parse '" ++ takeErrorSample input ++ "...' as a floating point number."
          rest = TL.unpack restPacked

traverseTree :: BinaryTree -> [Double] -> String
traverseTree Node {..} (thresholdHead:thresholds)
    | thresholdHead <= threshold = traverseTree left thresholds
    | thresholdHead > threshold  = traverseTree right thresholds
traverseTree Node {..} _ = error "Not enough values entered to perform classification."
traverseTree Leaf {..} _ = className

traverseTreeMultiple :: BinaryTree -> [[Double]] -> [Char]
traverseTreeMultiple tree (row:rows)
    | null rows = traverseTree tree row
    | otherwise = traverseTree tree row ++ "\n" ++ traverseTreeMultiple tree rows
traverseTreeMultiple _ [] = ""

utf8Print :: String -> IO ()
utf8Print = T.putStrLn . T.pack

parseValuesLine :: String -> Int -> ([Double], String)
parseValuesLine str line
    | end = ([value], restSpaces2)
    | otherwise = (value:values, rest)
    where (value, restDouble) = doubleOrError str line
          restSpaces1 = skipWhiteSpaces restDouble
          restComma
            | null restSpaces1 || head restSpaces1 == '\n' = restSpaces1
            | head restSpaces1 == ',' = tail restSpaces1
            | otherwise = myError line $ "Comma expected at: '" ++ takeErrorSample restSpaces1 ++ "...'"
          restSpaces2 = skipWhiteSpaces restComma
          end = null restSpaces2 || head restSpaces2 == '\n'
          (values, rest) = parseValuesLine restSpaces2 $ line + 1

parseValues :: String -> Int -> ([[Double]], String)
parseValues str line
    | null $ skipWhiteSpacesAndNewLines str = ([], [])
    | otherwise = (parsedRow:parsedRows, restLines)
    where (parsedRow, restLine) = parseValuesLine str line
          nextRow
            | null restLine = restLine
            | head restLine == '\n' = tail restLine
            | otherwise = myError line $ "Unexpected character at: '" ++ takeErrorSample restLine ++ "...'"
          (parsedRows, restLines) = parseValues nextRow $ line + 1


--parseArgs args
--    | length args == 2 = T.putStrLn "2 args"
--    | length args == 3 = T.putStrLn "3 args"

main :: IO ()
main = do
    --args  <- getArgs
    treeInput <- readFile "trees/example1.txt"
    valuesInput <- readFile "values/example1.txt"
    let [(tree, _)] = reads treeInput :: [(BinaryTree, String)]
    let (values, _) = parseValues valuesInput 0
    utf8Print $ show tree
    utf8Print $ show values
    utf8Print $ traverseTreeMultiple tree values
