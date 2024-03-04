{-# LANGUAGE RecordWildCards #-} -- to be able to use the created named data types conveniently

module Tree (
    BinaryTree(Node, Leaf), 
    traverseTree,
    traverseTreeMultiple,
    loadTrainCSV,
    trainTree,
    trainTreeMaxDepth
) where

import qualified Data.List as L
import Parsing (
    takeRestOfText, 
    skipWhiteSpaces, 
    formattedError, 
    takeErrorSample, 
    trim, 
    split, 
    checkRemoveIndent, 
    decimalOrLineError, 
    doubleOrLineError, 
    )

data BinaryTree = Node { index :: Int, threshold :: Double, leftTree :: BinaryTree, rightTree :: BinaryTree} | Leaf { className :: String }

instance Read BinaryTree where
    readsPrec _ input = [readTree input 0]
instance Show BinaryTree where
    show tree = treeString
        where treeString = init $ showBinaryTree tree 0

showBinaryTree :: BinaryTree -> Int -> String
showBinaryTree Node {..} depth = treeString
    where indent = take (2 * depth) [' ', ' ' ..]
          nextDepth = depth + 1
          leftRest = showBinaryTree leftTree nextDepth
          rightRest = showBinaryTree rightTree nextDepth
          treeString = indent ++ "Node: " ++ show index ++ ", " ++ show threshold ++ "\n" ++ leftRest ++ rightRest
showBinaryTree (Leaf className) depth = indent ++ "Leaf: " ++ className ++ "\n"
    where indent = take (2 * depth) [' ', ' ' ..]

readTree :: String -> Int -> (BinaryTree, String)
readTree ('N':'o':'d':'e':':':values) line = (Node parsedIndex parsedThreshold leftNode rightNode, restRight)
    where trimmed = skipWhiteSpaces values
          (parsedIndex, restIndex) = decimalOrLineError trimmed line
          (x:restSpaces) = skipWhiteSpaces restIndex
          check = (x == ',') || formattedError line ("Comma expected at: '" ++ takeErrorSample (x:restSpaces) ++ "...'")
          (parsedThreshold, restThreshold)
            | check = doubleOrLineError restSpaces line 
            | otherwise = (0, "") -- dummy
          (y:nextLineLeft) = skipWhiteSpaces restThreshold
          nextLineNum
            | y == '\n' = line + 1 
            | otherwise = formattedError line ("New line expected at: '" ++ takeErrorSample (y:nextLineLeft) ++ "...'")
          removedIndentLeft = checkRemoveIndent nextLineLeft nextLineNum
          (leftNode, restLeft) = readTree removedIndentLeft nextLineNum
          (z:nextLineRight) = skipWhiteSpaces restLeft
          removedIndentRight
            | z == '\n' = checkRemoveIndent nextLineRight nextLineNum
            | otherwise = formattedError line ("New line expected at: '" ++ takeErrorSample (z:nextLineRight) ++ "...'")
          (rightNode, restRight) = readTree removedIndentRight nextLineNum

readTree ('L':'e':'a':'f':':':values) _ = (Leaf className, rest)
    where trimmed = skipWhiteSpaces values
          (className, rest) = takeRestOfText trimmed
readTree wrong line = formattedError line $ "Unexpected key word at: '" ++ takeErrorSample wrong ++ "...'"

traverseTree :: BinaryTree -> [Double] -> String
traverseTree Node {..} values
    | length values <= index = error "Not enough values entered to perform classification."
    | values !! index <= threshold = traverseTree leftTree values
    | otherwise = traverseTree rightTree values
traverseTree Leaf {..} _ = className

traverseTreeMultiple :: BinaryTree -> [[Double]] -> String
traverseTreeMultiple tree (row:rows)
    | null rows = traverseTree tree row
    | otherwise = traverseTree tree row ++ "\n" ++ traverseTreeMultiple tree rows
traverseTreeMultiple _ [] = ""

gini :: [String] -> Double
gini samples = 1 - sumOccurences
    where classes = L.nub samples
          samplesLen = length samples
          occurences = [length $ filter (== x) samples | x <- classes]
          normPow2Occurences = map (\x -> (fromIntegral x / fromIntegral samplesLen)**2.0) occurences
          sumOccurences = sum normPow2Occurences

giniLR :: [(String, Int)] -> [(String, Int)] -> Double
giniLR leftSamples rightSamples = totalGini
    where leftLen = fromIntegral $ length leftSamples
          rightLen = fromIntegral $ length rightSamples
          totalLen = leftLen + rightLen
          giniLeft = gini $ map fst leftSamples
          giniRight = gini $ map fst rightSamples
          totalGini = leftLen * giniLeft / totalLen + rightLen * giniRight / totalLen

loadTrainCSV :: String -> Char -> ([[Double]], [(String, Int)])
loadTrainCSV strData separator = (featuresDouble, trimmedClasses)
    where colCount = length $ filter (== separator) $ takeWhile (/= '\n') strData
          rows = split '\n' strData
          allSameCols = all (== colCount) [length $ filter (== separator) row | row <- rows]
          (features, classes)
            | allSameCols = unzip [splitAt colCount $ split separator row | row <- rows]
            | otherwise = error "Entered CSV file does not have a valid structure, some rows have more columns than other."
          featuresDouble = L.transpose [map (\x -> fst $ doubleOrLineError x line) featureRow | (featureRow, line) <- zip features [0, 1 ..]]
          trimmedClasses = [(trim $ head classRow, idx) | (classRow, idx) <- zip classes [0, 1 ..]]

creteThresholds :: [[Double]] -> [[Double]]
creteThresholds selectedFeatures = ranges
    where sortedSelectedFeatures = map L.sort selectedFeatures
          ranges = [zipWith (\x y -> (x + y) / 2) feature (tail feature) | feature <- sortedSelectedFeatures]

mostOccurrences :: Ord a => [a] -> a
mostOccurrences list = fst $ foldr1 (\x y -> if snd x >= snd y then x else y) pairs
    where pairs = map (\x -> (head x, length x)) $ L.group $ L.sort list

trainTreeMaxDepth :: ([[Double]], [(String, Int)]) -> Int -> BinaryTree
trainTreeMaxDepth (features, classes) depth
    | depth == 0 = Leaf (mostOccurrences classNames)
    | length (L.nub classNames) == 1 = Leaf (head classNames)
    | otherwise = Node bestFeatureIdx bestThreshold (trainTreeMaxDepth (features, bestLeftClasses) nextDepth) (trainTreeMaxDepth (features, bestRightClasses) nextDepth)
    where nextDepth = depth - 1
          (classNames, indices) = unzip classes
          selectedFeatureTable = [[feature !! index | index <- indices] | feature <- features]
          thresholdTable = creteThresholds selectedFeatureTable
          classesSplit = \op -> [[[classPair 
                                    | (feature, classPair) <- zip selectedFeatures classes, op feature threshold]
                                        | threshold <- thresholds] 
                                            | (thresholds, selectedFeatures) <- zip thresholdTable selectedFeatureTable]
          leftClassesTables = classesSplit (<)
          rightClassesTables = classesSplit (>=)
          giniTable = [[(giniLR leftClasses rightClasses, leftClasses, rightClasses, threshold, featureIdx) 
                        | (leftClasses, rightClasses, threshold) <- zip3 leftClassesTable rightClassesTable thresholds] 
                            | (leftClassesTable, rightClassesTable, thresholds, featureIdx) <- L.zip4 leftClassesTables rightClassesTables thresholdTable [0, 1 ..]]
          flatGini = concat giniTable
          (_, bestLeftClasses, bestRightClasses, bestThreshold, bestFeatureIdx) = 
            foldr1 (\right@(x, _, _, _, _) left@(y, _, _, _, _) -> if x <= y then right else left) flatGini

trainTree :: ([[Double]], [(String, Int)]) -> BinaryTree
trainTree trainingData = trainTreeMaxDepth trainingData (length $ snd trainingData)
