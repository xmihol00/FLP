module Main (main) where

import qualified System.Environment as E
import Parsing (parseValues, utf8Print, decimalOrError)
import Tree (BinaryTree, traverseTreeMultiple, loadTrainCSV, trainTree, trainTreeMaxDepth)

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
    let tree = trainTree trainingData
    utf8Print $ show tree
mainTraining _ = error "Wrong number of parameters."

mainTrainingWithDepth :: [String] -> IO ()
mainTrainingWithDepth [trainingFile, depthStr] = do
    let (depth, _) = decimalOrError depthStr (error $ "Integer expected after the '-d' parameter, got: " ++ depthStr)
    trainingInput <- readFile trainingFile
    let trainingData = loadTrainCSV trainingInput ','
    let tree = trainTreeMaxDepth trainingData depth
    utf8Print $ show tree
mainTrainingWithDepth _ = error "Wrong number of parameters."

parseArgs :: [String] -> ([String] -> IO (), [String])
parseArgs [x] = (mainTraining, [x])
parseArgs [x, y] = (mainInference, [x, y])
parseArgs [x, "-d", depth] = (mainTrainingWithDepth, [x, depth])
parseArgs _ = error "Unexpected combination of command line arguments"

main :: IO ()
main = do
    args <- E.getArgs
    let (function, parsedArgs) = parseArgs args
    function parsedArgs
