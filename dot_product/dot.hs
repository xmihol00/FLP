import System.IO

arr1 = [1, 2, 3]
arr2 = [4, 5, 6]

dotProduct :: [Int] -> [Int] -> Int
dotProduct [] [] = 0
dotProduct (x:xs) (y:ys) = x * y + dotProduct xs ys

dotResult = dotProduct arr1 arr2

listMultiplication :: [Int] -> [Int] -> [Int]
listMultiplication [] [] = []
listMultiplication (x:xs) (y:ys) = x * y : listMultiplication xs ys

mulResult = listMultiplication arr1 arr2

