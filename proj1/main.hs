module Main (main) where

import qualified System.Environment as E
import Parsing (parseValues, utf8Print)
import Tree (BinaryTree, traverseTreeMultiple, loadTrainCSV, trainTree)

mainPrediction :: [FilePath] -> IO ()
mainPrediction [treeFile, valuesFile] = do
    treeInput <- readFile treeFile
    valuesInput <- readFile valuesFile
    let [(tree, _)] = reads treeInput :: [(BinaryTree, String)]
    let (values, _) = parseValues valuesInput 0
    utf8Print $ traverseTreeMultiple tree values
mainPrediction _ = error "Wrong number of parameters."

mainTraining :: [FilePath] -> IO ()
mainTraining [trainingFile] = do
    trainingInput <- readFile trainingFile
    let trainingData = loadTrainCSV trainingInput ','
    let tree = trainTree trainingData
    utf8Print $ show tree
mainTraining _ = error "Wrong number of parameters."

main :: IO ()
main = do
    args  <- E.getArgs
    case length args of
        1 -> mainTraining args
        2 -> mainPrediction args
        _ -> error "Unexpected number of arguments."
