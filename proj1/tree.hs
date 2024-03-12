--------------------------------------------------------------------------------------------------------------------
--   project: flp-fun (1st project regarding decision trees to Functional and Logic Programming course at FIT, BUT)
--    author: David Mihola (xmihol00)
--     email: xmihol00@stud.fit.vutbr.cz
--      date: 31. 3. 2024
-- file info: Tree data type and function for reading a tree from file, performing inference on a previously
--            trained tree and training a new tree from given data.
--------------------------------------------------------------------------------------------------------------------

{-# LANGUAGE RecordWildCards #-} -- to be able to use the created named data types conveniently

module Tree (
    BinaryTree(Node, Leaf),
    traverseTree,
    traverseTreeMultiple,
    loadTrainCSV,
    trainTreeMaxDepth,
    trainTreeMinSamplesSplit,
    trainTreeMinSamplesLeaf,
    trainTreeMaxDepthMinSamplesSplit,
    trainTreeMaxDepthMinSamplesLeaf,
    trainTreeMinSamplesSplitMinSamplesLeaf,
    trainTreeBasic,
    trainTreeMemoryInefficient,
    trainTree
) where

import qualified Data.Char as C
import qualified Data.List as L
import Parsing (
    takeRestOfLine,
    skipWhiteSpaces,
    formattedError,
    takeErrorSample,
    trim,
    split,
    checkRemoveIndent,
    integerOrLineError,
    doubleOrLineError,
    skipWhiteSpacesAndNewLines
    )

data BinaryTree = Node { index :: Int, threshold :: Double, leftTree :: BinaryTree, rightTree :: BinaryTree} | Leaf { className :: String }

-- enable read and show functions to work on the BinaryTree data type
instance Read BinaryTree where
    readsPrec _ input = [readTree input]
instance Show BinaryTree where
    show tree = treeString
        where treeString = init $ showBinaryTree tree 0 -- use init to discard the last 'optimistic' empty line, see showBinaryTree for a Leaf

showBinaryTree :: BinaryTree -> Int -> String
showBinaryTree Node {..} depth = treeString
    where
        -- generate indent based on the depth of the tree 
        indent = take (2 * depth) [' ', ' ' ..]
        -- increase the depth
        nextDepth = depth + 1
        -- generate the left subtree
        leftRest = showBinaryTree leftTree nextDepth
        -- generate the right subtree
        rightRest = showBinaryTree rightTree nextDepth
        -- concatenate the values of the current node with the indent subtrees
        treeString = indent ++ "Node: " ++ show index ++ ", " ++ show threshold ++ "\n" ++ leftRest ++ rightRest

showBinaryTree (Leaf className) depth = indent ++ "Leaf: " ++ className ++ "\n" -- generate correctly indented leaf node and expect the tree to continue at a new line
    where indent = take (2 * depth) [' ', ' ' ..]

readTree :: String -> (BinaryTree, String)
readTree fileString
    | null nothingElse = (tree, nothingElse)
    | otherwise = formattedError line "Unexpected characters occurred in the file after parsing of the tree."
    where
        -- parse tree
        (tree, rest, line) = readTreeHelper fileString 0 0
        -- check that the file contains only white spaces after the tree
        nothingElse = skipWhiteSpacesAndNewLines rest

readTreeHelper :: [Char] -> Int -> Int -> (BinaryTree, String, Int)
readTreeHelper ('N':'o':'d':'e':':':values) lineNum indent = (Node parsedIndex parsedThreshold leftNode rightNode, restRight, lastLineNum)
    where 
        nextIndent = indent + 1
        -- skip leading white spaces apart from new lines and parse the expected integer after
        (parsedIndex, restIndex) = integerOrLineError values lineNum
        -- skip white spaces apart from new lines after the parsed integer
        (x:restSpaces) = skipWhiteSpaces restIndex
        -- check that the index and threshold are separated by a ',', skip leading white spaces apart from new lines and parse the expected double after 
        (parsedThreshold, restThreshold)
          | x == ',' = doubleOrLineError restSpaces lineNum
          | otherwise = formattedError lineNum ("Comma expected at: '" ++ takeErrorSample (x:restSpaces) ++ "...'")
        -- skip white spaces apart from new lines after the parsed double
        (y:nextLineLeft) = skipWhiteSpaces restThreshold
        -- check that the line ends without any non-white space characters, remove the indent at the next line and verify its correct length
        removedIndentLeft
          | y /= '\n' = formattedError lineNum ("New line expected at: '" ++ takeErrorSample (y:nextLineLeft) ++ "...'")
          | otherwise = checkRemoveIndent nextLineLeft nextIndent (lineNum + 1)
        -- parse the left subtree
        (leftNode, restLeft, nextLineNum) = readTreeHelper removedIndentLeft (lineNum + 1) nextIndent
        (z:nextLineRight) = skipWhiteSpaces restLeft
        -- check that the line ends without any non-white space characters, remove the indent at the next line and verify its correct length
        removedIndentRight
          | z /= '\n' = formattedError nextLineNum ("New line expected at: '" ++ takeErrorSample (z:nextLineRight) ++ "...'")
          | otherwise = checkRemoveIndent nextLineRight nextIndent nextLineNum
        -- parse the right subtree
        (rightNode, restRight, lastLineNum) = readTreeHelper removedIndentRight nextLineNum nextIndent

readTreeHelper ('L':'e':'a':'f':':':values) lineNum _ = (Leaf className, rest, lineNum + 1)
    where 
        -- read until the '\n' character
        (restOfLine, rest) = takeRestOfLine values
        -- trim the string, but allow spaces in the class name
        className = trim restOfLine

-- deal with some other possible errors
readTreeHelper wrong@(x:_) lineNum _
    | C.isSpace x = formattedError lineNum $ "Unexpected indent."
    | otherwise = formattedError lineNum $ "Unexpected key word at: '" ++ takeErrorSample wrong ++ "...'"
readTreeHelper [] 0 _ = formattedError 0 $ "Nothing to be parsed in the tree file, check the content of the file input."
readTreeHelper [] _ _ = formattedError (-1) $ "An unexpected error occurred while parsing the tree file, verify correct formatting of the file, please."

traverseTree :: BinaryTree -> [Double] -> String
traverseTree Node {..} values
    | length values <= index = error "Not enough values entered to perform classification." -- TODO take the most likely
    | values !! index <= threshold = traverseTree leftTree values -- look for the class in the left subtree
    | otherwise = traverseTree rightTree values -- otherwise look for the class in the right subtree
traverseTree Leaf {..} _ = className

traverseTreeMultiple :: BinaryTree -> [[Double]] -> String
traverseTreeMultiple tree (row:rows)
    | null rows = traverseTree tree row
    | otherwise = traverseTree tree row ++ "\n" ++ traverseTreeMultiple tree rows
traverseTreeMultiple _ [] = ""

gini :: [String] -> Double
gini samples = giniValue
    where 
        -- get unique classes
        classes = L.nub samples
        -- get the normalization factor
        samplesLen = length samples
        -- compute number of occurences of each unique class
        occurences = [length $ filter (== x) samples | x <- classes]
        -- compute (<class occurrence>/<number of samples>)^2
        normPow2Occurences = map (\x -> (fromIntegral x / fromIntegral samplesLen)**2.0) occurences
        -- sum the total
        sumOccurences = sum normPow2Occurences
        -- use the inverse as the gini value, i.e. giniValue = 1 - sum_{over all classes}((<class occurrence in samples>/<number of samples>)^2)
        giniValue = 1 - sumOccurences

giniLR :: ([(String, Int)], [(String, Int)]) -> Double
giniLR (leftSamples, rightSamples) = totalGini
    where
        -- normalization factor of left list as double
        leftLen = fromIntegral $ length leftSamples
        -- normalization factor of right list as double
        rightLen = fromIntegral $ length rightSamples
        -- compute the overall normalization factor
        totalLen = leftLen + rightLen
        -- compute gini values for each list
        giniLeft = gini $ map fst leftSamples
        giniRight = gini $ map fst rightSamples
        -- compute the total gini value as a weighted sum of the gini value of the separate lists
        totalGini = leftLen * giniLeft / totalLen + rightLen * giniRight / totalLen

loadTrainCSV :: String -> Char -> ([[Double]], [(String, Int)])
loadTrainCSV strData separator = (featuresDouble, trimmedClasses)
    where 
        -- expect the first row to have the same schema as all other rows, i.e. number of columns
        colCount = length $ filter (== separator) $ takeWhile (/= '\n') strData
        -- split the file into lines
        rows = split '\n' strData
        -- ensure the same row schema (same number of columns) over the whole file
        allSameCols = all (== colCount) [length $ filter (== separator) row | row <- rows]
        (features, classes)
          | not allSameCols = error "Entered CSV file does not have a valid structure, some rows have more columns than other."
          -- split each row to X (training data) and y (expected class)
          | otherwise = unzip [splitAt colCount $ split separator row | row <- rows] -- first split on the separator, second split X and y
        -- convert features to doubles while ignoring all leading white spaces and transpose then in order to have the data better prepared for training
        featuresDouble = L.transpose [map (\x -> fst $ doubleOrLineError x line) featureRow | (featureRow, line) <- zip features [0, 1 ..]]
        -- trim leading and trailing white spaces form the class names and add pair them with their row indices
        trimmedClasses = [(trim $ head classRow, idx) | (classRow, idx) <- zip classes [0, 1 ..]]

creteThresholds :: [[Double]] -> [[Double]]
creteThresholds selectedFeatures = thresholds
    where 
        sortedSelectedFeatures = map L.sort selectedFeatures
        -- threshold is regarded as an arithmetic mean between two consecutive feature values, 
        -- this can be achieved by zipping the sorted list and its tail
        thresholds = [zipWith (\x y -> (x + y) / 2) feature (tail feature) | feature <- sortedSelectedFeatures]

mostOccurrences :: Ord a => [a] -> a
mostOccurrences list = mostOccurant
    where 
        -- first sort the list, second group the same sequences, third create pairs with 'a' and number of its occurrences
        pairs = map (\x -> (head x, length x)) $ L.group $ L.sort list
        -- fourth use foldr for maximum reduction, fifth take just the 'a' and disregard the number of occurrences
        -- note: both 'x2 >= y2' and 'x2 > y2' works
        mostOccurant = fst $ foldr1 (\x@(_, x2) y@(_, y2) -> if x2 >= y2 then x else y) pairs

-- inefficient training, generates all possible solutions for each node and then reduces them to the best one
trainTreeMemoryInefficient :: (Int, Int, Int) -> ([[Double]], [(String, Int)]) -> BinaryTree
trainTreeMemoryInefficient (depth, minSamplesSplit, minSamplesLeaf) (features, classes)
    | depth == 0 = Leaf (mostOccurrences classNames)
    | length classNames <= minSamplesSplit = Leaf (mostOccurrences classNames)
    | length bestLeftClasses < minSamplesLeaf || length bestRightClasses < minSamplesLeaf = Leaf (mostOccurrences classNames)
    | length (L.nub classNames) == 1 = Leaf (head classNames)
    | otherwise = Node bestFeatureIdx bestThreshold (trainTreeMemoryInefficient nextParams (features, bestLeftClasses)) (trainTreeMemoryInefficient nextParams (features, bestRightClasses))
    where 
        nextParams = (depth - 1, minSamplesSplit, minSamplesLeaf)
        (classNames, indices) = unzip classes
        -- filter still relevant features
        selectedFeatureTable = [[feature !! index | index <- indices] | feature <- features]
        -- compute thresholds from relevant features
        thresholdTable = creteThresholds selectedFeatureTable
        -- partitioning function
        classesSplit = \op -> [[[classPair
                                  | (feature, classPair) <- zip selectedFeatures classes, op feature threshold]
                                      | threshold <- thresholds]
                                          | (thresholds, selectedFeatures) <- zip thresholdTable selectedFeatureTable]
        -- perform partitioning based on the relation to threshold
        leftClassesTables = classesSplit (<)
        rightClassesTables = classesSplit (>=)
        -- generating all possible solutions
        giniTable = [[(giniLR (leftClasses, rightClasses), leftClasses, rightClasses, threshold, featureIdx)
                      | (leftClasses, rightClasses, threshold) <- zip3 leftClassesTable rightClassesTable thresholds]
                          | (leftClassesTable, rightClassesTable, thresholds, featureIdx) <- L.zip4 leftClassesTables rightClassesTables thresholdTable [0, 1 ..]]
        giniFlat = concat giniTable
        -- finding the best solution (use flodr for minimum reduction)
        (_, bestLeftClasses, bestRightClasses, bestThreshold, bestFeatureIdx) =
            foldr1 (\right@(x, _, _, _, _) left@(y, _, _, _, _) -> if x <= y then right else left) giniFlat

giniZip :: ([(String, Int)], [(String, Int)]) -> Double -> Int -> (Double, [(String, Int)], [(String, Int)], Double, Int)
giniZip list@(leftList, rightList) threshold index = (giniLR list, leftList, rightList, threshold, index)
 
giniReduce :: [(Double, [(String, Int)], [(String, Int)], Double, Int)] -> (Double, [(String, Int)], [(String, Int)], Double, Int)
-- use of foldr for minimum reduction
giniReduce = foldr1 (\right@(x, _, _, _, _) left@(y, _, _, _, _) -> if x <= y then right else left)

thresholdPartition :: Double -> [(Double, (String, Int))] -> ([(String, Int)], [(String, Int)])
-- partition the list based on a threshold into two list in one linear pass using foldr and starting from empty lists
thresholdPartition threshold = foldr (\(feature, classPair) (leftList, rightList) -> 
    if feature < threshold then (classPair:leftList, rightList) else (leftList, classPair:rightList)) ([], [])

-- memory efficient solution, which reduces partially to the best solution
trainTree :: (Int, Int, Int) -> ([[Double]], [(String, Int)]) -> BinaryTree
trainTree (depth, minSamplesSplit, minSamplesLeaf) (features, classes)
     -- maximum depth reached, node must become Leaf
    | depth == 0 = Leaf (mostOccurrences classNames)
     -- too small node to be split, must become Leaf
    | length classNames <= minSamplesSplit = Leaf (mostOccurrences classNames)
    -- left subtree or right subtree would be to small, already this node must become Leaf
    | length bestLeftClasses < minSamplesLeaf || length bestRightClasses < minSamplesLeaf = Leaf (mostOccurrences classNames)
    -- only one class left in this node, i.e. it is a Leaf node
    | length (L.nub classNames) == 1 = Leaf (head classNames)
    -- split the node
    | otherwise = Node bestFeatureIdx bestThreshold (trainTree nextParams (features, bestLeftClasses)) (trainTree nextParams (features, bestRightClasses))
    where 
        nextParams = (depth - 1, minSamplesSplit, minSamplesLeaf)
        (classNames, indices) = unzip classes
        -- filter still relevant features 
        selectedFeatureTable = [[feature !! index | index <- indices] | feature <- features]
        -- compute thresholds from relevant features
        thresholdTable = creteThresholds selectedFeatureTable
        -- reduce generated solutions in batches
        (_, bestLeftClasses, bestRightClasses, bestThreshold, bestFeatureIdx) = 
          giniReduce [ -- reduce to the best overall
              giniReduce [ -- reduce to the best for each feature index
                  giniZip (thresholdPartition threshold (zip selectedFeatures classes)) threshold featureIdx | threshold <- thresholds
              ] | (thresholds, selectedFeatures, featureIdx) <- L.zip3 thresholdTable selectedFeatureTable [0, 1 ..]
          ]

-- fill in default values to make specific functions
trainTreeMaxDepth :: Int -> ([[Double]], [(String, Int)]) -> BinaryTree
trainTreeMaxDepth depth = trainTree (depth, 2, 1)

trainTreeMinSamplesSplit :: Int -> ([[Double]], [(String, Int)]) -> BinaryTree
trainTreeMinSamplesSplit minSamplesSplit = trainTree (maxBound, minSamplesSplit, 1)

trainTreeMinSamplesLeaf :: Int -> ([[Double]], [(String, Int)]) -> BinaryTree
trainTreeMinSamplesLeaf minSamplesLeaf = trainTree (maxBound, 2, minSamplesLeaf)

trainTreeMaxDepthMinSamplesSplit :: (Int, Int) -> ([[Double]], [(String, Int)]) -> BinaryTree
trainTreeMaxDepthMinSamplesSplit (depth, minSamplesSplit) = trainTree (depth, minSamplesSplit, 1)

trainTreeMaxDepthMinSamplesLeaf :: (Int, Int) -> ([[Double]], [(String, Int)]) -> BinaryTree
trainTreeMaxDepthMinSamplesLeaf (depth, minSamplesLeaf) = trainTree (depth, 2, minSamplesLeaf)

trainTreeMinSamplesSplitMinSamplesLeaf :: (Int, Int) -> ([[Double]], [(String, Int)]) -> BinaryTree
trainTreeMinSamplesSplitMinSamplesLeaf (minSamplesSplit, minSamplesLeaf) = trainTree (maxBound, minSamplesSplit, minSamplesLeaf)

trainTreeBasic :: ([[Double]], [(String, Int)]) -> BinaryTree
trainTreeBasic = trainTree (maxBound, 2, 1)
