{-# LANGUAGE RecordWildCards #-}

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Read as TL
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Char as C

--module Main (main) where
--import Tree (BinaryTree(Node, Leaf))

data BinaryTree = Node { key :: Int, threshold :: Double, left :: BinaryTree, right :: BinaryTree} | Leaf { className :: String }

instance Read BinaryTree where
    readsPrec _ input = [readTree input 0]
instance Show BinaryTree where
    show tree = treeString
        where treeString = init $ showBinaryTree tree 0

showBinaryTree :: BinaryTree -> Int -> String
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

skipWhiteSpacesAndNewLines :: String -> String
skipWhiteSpacesAndNewLines = dropWhile (\x -> x == ' ' || x == '\t' || x == '\n')

takeErrorSample :: String -> String
takeErrorSample = takeWhile (/= '\n') . take 16

myError :: (Show a1, Num a1) => a1 -> String -> a2
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

traverseTreeMultiple :: BinaryTree -> [[Double]] -> String
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

gini :: [String] -> Double
gini samples = 1 - sumOccurences
    where classes = L.nub samples
          samplesLen = length samples
          occurences = [length $ filter (== x) samples | x <- classes]
          normPow2Occurences = map (\x -> (fromIntegral x / fromIntegral samplesLen)^2) occurences
          sumOccurences = sum normPow2Occurences

giniLR :: [(String, Int)] -> [(String, Int)] -> Double
giniLR leftSamples rightSamples = totalGini
    where leftLen = fromIntegral $ length leftSamples
          rightLen = fromIntegral $ length rightSamples
          totalLen = leftLen + rightLen
          giniLeft = gini $ map fst leftSamples
          giniRight = gini $ map fst rightSamples
          totalGini = leftLen * giniLeft / totalLen + rightLen * giniRight / totalLen


split :: Char -> String -> [String]
split delimiter strData
    | null strData || all ((==True) . C.isSpace) strData = []
    | otherwise = start : split delimiter rest
    where (start, nlRest) = span (/= delimiter) strData
          rest
            | null nlRest = []
            | otherwise = tail nlRest

trim :: [Char] -> [Char]
trim str = trimEnd
    where trimmedStart = dropWhile C.isSpace str
          trimEnd = reverse $ dropWhile C.isSpace $ reverse trimmedStart

loadTrainCSV :: String -> Char -> ([[Double]], [(String, Int)])
loadTrainCSV strData separator = (featuresDouble, trimmedClasses)
    where colCount = length $ filter (== separator) $ takeWhile (/= '\n') strData
          rows = split '\n' strData
          allSameCols = all (== colCount) [length $ filter (== separator) row | row <- rows]
          (features, classes)
            | allSameCols = unzip [splitAt colCount $ split separator row | row <- rows]
            | otherwise = error "Entered CSV file does not have a valid structure, some rows have more columns than other."
          featuresDouble = L.transpose [map (\x -> fst $ doubleOrError x line) featureRow | (featureRow, line) <- zip features [0, 1 ..]]
          trimmedClasses = [(trim $ head classRow, idx) | (classRow, idx) <- zip classes [0, 1 ..]]

creteThresholds :: [[Double]] -> [[Double]]
creteThresholds selectedFeatures = ranges
    where sortedSelectedFeatures = map L.sort selectedFeatures
          ranges = [zipWith (\x y -> (x+y)/2) feature (tail feature) | feature <- sortedSelectedFeatures]

reduce :: (t -> t -> Bool) -> t -> [t] -> t
reduce op best [] = best
reduce op best [x] = if op best x then best else x
reduce op best (x:rest) = if op best x then reduce op best rest else reduce op x rest

trainTree :: ([[Double]], [(String, Int)]) -> BinaryTree
trainTree trainingData@(features, classes)
    | null (L.nub classNames) = error "TODO"
    | length (L.nub classNames) == 1 = Leaf (head classNames)
    | otherwise = Node featureIdx threshold (trainTree (features, leftClasses)) (trainTree (features, rightClasses))
    where (classNames, indices) = unzip classes
          selectedFeatureTable = [[feature !! index | index <- indices] | feature <- features]
          thresholdTable = creteThresholds selectedFeatureTable
          classesSplit = \op -> [[[classPair | (feature, classPair) <- zip selectedFeatures classes, op feature threshold] | selectedFeatures <- selectedFeatureTable, threshold <- thresholds] | thresholds <- thresholdTable]
          leftClassesTables = classesSplit (<)
          rightClassesTables = classesSplit (>=)
          giniTable = [[(giniLR leftClasses rightClasses, leftClasses, rightClasses, threshold, featureIdx) | (leftClasses, rightClasses, threshold) <- zip3 leftClassesTable rightClassesTable thresholds] | (leftClassesTable, rightClassesTable, thresholds, featureIdx) <- L.zip4 leftClassesTables rightClassesTables thresholdTable [0, 1 ..]]
          flatGini = concat giniTable
          (gini, leftClasses, rightClasses, threshold, featureIdx) = reduce (\(x, _, _, _, _) (y, _, _, _, _) -> x < y) (head flatGini) (tail flatGini)




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
    trainingInput <- readFile "training_data/example2.txt"
    let trainingData = loadTrainCSV trainingInput ','
    utf8Print $ show tree
    utf8Print $ show values
    utf8Print $ traverseTreeMultiple tree values
    utf8Print $ show trainingData
    let tree = trainTree trainingData
    utf8Print $ show tree
