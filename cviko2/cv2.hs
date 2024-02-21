import qualified Data.Map as Map
import Data.List

-- zapis klasickeho zip pomocí zipWith
zip' = zipWith (\x y -> (x, y))
zip'' = zipWith (,)

-- filter pomocí foldr/foldl
filter' q = foldr (\x y -> if q x then x:y else y) []

-- map pomocí foldr/foldl
map' f = foldr (\x y -> (f x) : y) []

pointsL = [("xmihol00", 1), ("xborec99", 5), ("xblbec-1", 0)]
pointsM = Map.fromList pointsL

-- pomocna funkce pro overeni typu Either
isSomething (Just _) = True 
isSomething _ = False

-- pomocna funkce pro extrahovani hodnoty z Just 
getJust (Just a) = a

-- funkce, ktera ziska pocet bodu studenta a pricte 1 a vrati (Right vysledku), 
-- pripadne vrati (Left "max points"), pokud student ma jiz 5 bodu. 
-- Pokud student se zadanym loginem neexistuje vrati (Left "unknown login")
activity login students = answer  
    where found = Map.lookup login students
          answer = if isSomething found then 
                        if (getJust found) < 5 
                        then Right ((getJust found) + 1) 
                        else Left "max points" 
                   else Left "unknown login"
