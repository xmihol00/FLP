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
mainTrainingArgs [trainingFile, depthStr, minSamplesSplitStr, minSamplesLeafStr] = do
    let (depth, _) = integerOrError depthStr (error $ "Integer expected after the '-d' parameter, got: " ++ depthStr)
    let (minSamplesSplit, _) = integerOrError minSamplesSplitStr (error $ "Integer expected after the '-mss' parameter, got: " ++ minSamplesSplitStr)
    let (minSamplesLeaf, _) = integerOrError minSamplesLeafStr (error $ "Integer expected after the '-msf' parameter, got: " ++ minSamplesLeafStr)
    trainingInput <- readFile trainingFile
    let trainingData = loadTrainCSV trainingInput ','
    let tree = trainTree (depth, minSamplesSplit, minSamplesLeaf) trainingData
    utf8Print $ show tree
mainTrainingArgs _ = error "Wrong parameters."

mainEchoTree :: [String] -> IO ()
mainEchoTree [treeFile] = do
    treeInput <- readFile treeFile
    let [(tree, _)] = reads treeInput :: [(BinaryTree, String)]
    utf8Print $ show tree
mainEchoTree _ = error "Wrong parameters."

-- somewhat cumbersome parsing of command line parameters, but it works for this purpose, normally some library would be used instead
-- default values: depth=INT_MAX, minSamplesSplit=2, minSamplesLeaf=1
parseArgs :: [String] -> ([String] -> IO (), [String])
parseArgs [x, "--echo_tree"] = (mainEchoTree, [x])

parseArgs [x] = (mainTraining, [x])
parseArgs [x, y] = (mainInference, [x, y])

parseArgs [x, "-d", depth] = (mainTrainingArgs, [x, depth, "2", "1"])
parseArgs [x, "-mss", minSamplesSplit] = (mainTrainingArgs, [x, show (maxBound :: Int), minSamplesSplit, "1"])
parseArgs [x, "-msl", minSamplesLeaf] = (mainTrainingArgs, [x, show (maxBound :: Int), "2", minSamplesLeaf])

parseArgs [x, "-d", depth, "-mss", minSamplesSplit] = (mainTrainingArgs, [x, depth, minSamplesSplit, "1"])
parseArgs [x, "-mss", minSamplesSplit, "-d", depth] = parseArgs [x, "-d", depth, "-mss", minSamplesSplit]
parseArgs [x, "-d", depth, "-msf", minSamplesLeaf] = (mainTrainingArgs, [x, depth, "2", minSamplesLeaf])
parseArgs [x, "-msf", minSamplesLeaf, "-d", depth] = parseArgs [x, "-d", depth, "-msf", minSamplesLeaf]
parseArgs [x, "-mss", minSamplesSplit, "-msf", minSamplesLeaf] = (mainTrainingArgs, [x, show (maxBound :: Int), minSamplesSplit, minSamplesLeaf])
parseArgs [x, "-msf", minSamplesLeaf, "-mss", minSamplesSplit] = parseArgs [x, "-mss", minSamplesSplit, "-msf", minSamplesLeaf]

parseArgs [x, "-d", depth, "-mss", minSamplesSplit, "-msf", minSamplesLeaf] = (mainTrainingArgs, [x, depth, minSamplesSplit, minSamplesLeaf])
parseArgs [x, "-d", depth, "-msf", minSamplesLeaf, "-mss", minSamplesSplit] = parseArgs [x, "-d", depth, "-mss", minSamplesSplit, "-msf", minSamplesLeaf]
parseArgs [x, "-mss", minSamplesSplit, "-d", depth, "-msf", minSamplesLeaf] = parseArgs [x, "-d", depth, "-mss", minSamplesSplit, "-msf", minSamplesLeaf]
parseArgs [x, "-mss", minSamplesSplit, "-msf", minSamplesLeaf, "-d", depth] = parseArgs [x, "-d", depth, "-mss", minSamplesSplit, "-msf", minSamplesLeaf]
parseArgs [x, "-msf", minSamplesLeaf, "-d", depth, "-mss", minSamplesSplit] = parseArgs [x, "-d", depth, "-mss", minSamplesSplit, "-msf", minSamplesLeaf]
parseArgs [x, "-msf", minSamplesLeaf, "-mss", minSamplesSplit, "-d", depth] = parseArgs [x, "-d", depth, "-mss", minSamplesSplit, "-msf", minSamplesLeaf]

parseArgs _ = error "Unexpected combination of command line arguments"

main :: IO ()
main = do
    args <- E.getArgs
    let (function, parsedArgs) = parseArgs args
    function parsedArgs
