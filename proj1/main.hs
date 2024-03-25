--------------------------------------------------------------------------------------------------------------------
--   project: flp-fun (1st project regarding decision trees to Functional and Logic Programming course at FIT, BUT)
--    author: David Mihola (xmihol00)
--     email: xmihol00@stud.fit.vutbr.cz
--      date: 31. 3. 2024
-- file info: Parsing of inputs, reading of files and performing the desired actions based on passed parameters.
--------------------------------------------------------------------------------------------------------------------

module Main (main) where

import qualified System.Environment as E
import Parsing (parseValues, utf8Print, integerOrError)
import Tree (BinaryTree, traverseTreeMultiple, loadTrainCSV, trainTree, trainTreeBasic)

mainInference :: [String] -> IO ()
mainInference [treeFile, valuesFile] = do
    treeInput <- readFile treeFile
    valuesInput <- readFile valuesFile
    let [(tree, _)] = reads treeInput :: [(BinaryTree, String)]
    let (values, _) = parseValues valuesInput 0
    utf8Print $ traverseTreeMultiple tree values
mainInference _ = error "Wrong number of parameters."

mainTraining :: [String] -> IO ()
mainTraining [trainingFile] = do
    trainingInput <- readFile trainingFile
    let trainingData = loadTrainCSV trainingInput ','
    let tree = trainTreeBasic trainingData
    utf8Print $ show tree
mainTraining _ = error "Wrong number of parameters."

mainTrainingArgs :: [String] -> IO ()
mainTrainingArgs [trainingFile, maxDepthStr, minSamplesSplitStr, minSamplesLeafStr] = do
    let (maxDepth, _) = integerOrError maxDepthStr (error $ "Integer expected after the '-md' parameter, got: " ++ maxDepthStr)
    let (minSamplesSplit, _) = integerOrError minSamplesSplitStr (error $ "Integer expected after the '-mss' parameter, got: " ++ minSamplesSplitStr)
    let (minSamplesLeaf, _) = integerOrError minSamplesLeafStr (error $ "Integer expected after the '-msl' parameter, got: " ++ minSamplesLeafStr)
    trainingInput <- readFile trainingFile
    let trainingData = loadTrainCSV trainingInput ','
    let tree = trainTree (maxDepth, minSamplesSplit, minSamplesLeaf) trainingData
    utf8Print $ show tree
mainTrainingArgs _ = error "Wrong parameters."

mainEchoTree :: [String] -> IO ()
mainEchoTree [treeFile] = do
    treeInput <- readFile treeFile
    let [(tree, _)] = reads treeInput :: [(BinaryTree, String)]
    utf8Print $ show tree
mainEchoTree _ = error "Wrong parameters."

-- somewhat cumbersome parsing of command line parameters, but it works for this purpose, normally some library would be used instead
-- default values: maxDepth=INT_MAX, minSamplesSplit=2, minSamplesLeaf=1
parseArgs :: [String] -> ([String] -> IO (), [String])
parseArgs [x, "--echo_tree"] = (mainEchoTree, [x])

parseArgs ["-2", x] = (mainTraining, [x])
parseArgs ["-1", x, y] = (mainInference, [x, y])

parseArgs ["-2", x, "-md", maxDepth] = (mainTrainingArgs, [x, maxDepth, "2", "1"])
parseArgs ["-2", x, "-mss", minSamplesSplit] = (mainTrainingArgs, [x, show (maxBound :: Int), minSamplesSplit, "1"])
parseArgs ["-2", x, "-msl", minSamplesLeaf] = (mainTrainingArgs, [x, show (maxBound :: Int), "2", minSamplesLeaf])

parseArgs ["-2", x, "-md", maxDepth, "-mss", minSamplesSplit] = (mainTrainingArgs, [x, maxDepth, minSamplesSplit, "1"])
parseArgs ["-2", x, "-mss", minSamplesSplit, "-md", maxDepth] = parseArgs ["-2", x, "-md", maxDepth, "-mss", minSamplesSplit]
parseArgs ["-2", x, "-md", maxDepth, "-msl", minSamplesLeaf] = (mainTrainingArgs, [x, maxDepth, "2", minSamplesLeaf])
parseArgs ["-2", x, "-msl", minSamplesLeaf, "-md", maxDepth] = parseArgs ["-2", x, "-md", maxDepth, "-msl", minSamplesLeaf]
parseArgs ["-2", x, "-mss", minSamplesSplit, "-msl", minSamplesLeaf] = (mainTrainingArgs, [x, show (maxBound :: Int), minSamplesSplit, minSamplesLeaf])
parseArgs ["-2", x, "-msl", minSamplesLeaf, "-mss", minSamplesSplit] = parseArgs ["-2", x, "-mss", minSamplesSplit, "-msl", minSamplesLeaf]

parseArgs ["-2", x, "-md", maxDepth, "-mss", minSamplesSplit, "-msl", minSamplesLeaf] = (mainTrainingArgs, [x, maxDepth, minSamplesSplit, minSamplesLeaf])
parseArgs ["-2", x, "-md", maxDepth, "-msl", minSamplesLeaf, "-mss", minSamplesSplit] = parseArgs ["-2", x, "-md", maxDepth, "-mss", minSamplesSplit, "-msl", minSamplesLeaf]
parseArgs ["-2", x, "-mss", minSamplesSplit, "-md", maxDepth, "-msl", minSamplesLeaf] = parseArgs ["-2", x, "-md", maxDepth, "-mss", minSamplesSplit, "-msl", minSamplesLeaf]
parseArgs ["-2", x, "-mss", minSamplesSplit, "-msl", minSamplesLeaf, "-md", maxDepth] = parseArgs ["-2", x, "-md", maxDepth, "-mss", minSamplesSplit, "-msl", minSamplesLeaf]
parseArgs ["-2", x, "-msl", minSamplesLeaf, "-md", maxDepth, "-mss", minSamplesSplit] = parseArgs ["-2", x, "-md", maxDepth, "-mss", minSamplesSplit, "-msl", minSamplesLeaf]
parseArgs ["-2", x, "-msl", minSamplesLeaf, "-mss", minSamplesSplit, "-md", maxDepth] = parseArgs ["-2", x, "-md", maxDepth, "-mss", minSamplesSplit, "-msl", minSamplesLeaf]

parseArgs xs = error $ "Unexpected combination of command line arguments: " ++ show xs

main :: IO ()
main = do
    args <- E.getArgs
    let (function, parsedArgs) = parseArgs args
    -- execute the required function based on given parameters
    function parsedArgs
