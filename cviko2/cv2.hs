import qualified Data.Map as Map
import Data.List

zip' = zipWith (\x y -> (x, y))

zip'' = zipWith (,)

filter' q = foldr (\x y -> if q x then x:y else y) []

map' f = foldr (\x y -> (f x) : y) []

pointsL = [("aa", 1), ("bb", 0), ("cc", 5)]
pointsM = Map.fromList pointsL
pM = Map.fromList [(1, "aa"), (0, "bb"), (5, "cc")]

isSomething (Just _) = True 
isSomething _ = False

getJust (Just a) = a

activity l p = answer  
    where found = Map.lookup l p
          answer = if isSomething found then 
                        if (getJust found) < 5 
                        then Right ((getJust found) + 1) 
                        else Left "max points" 
                   else Left "unknown login"