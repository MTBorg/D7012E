-- hide prelude modules to avoid conflicts
import           Prelude                 hiding ( sum )

sum = foldr (+) 0

subL :: [Int] -> [(Int, Int, [Int])]
subL (x : xs)
    | xs == [] = [(0, 0, [x])]
    | otherwise =  [(0, 0, [x])]
    ++ map (\(i, j, l) -> (i, j + 1, x : l)) (subL xs)

allSubLists :: [Int] -> [(Int, Int, [Int])]
allSubLists (x : xs) | xs == []  = [(0, 0, [x])]
                     | otherwise = subL (x : xs) 
                     ++ map (\(i,j,l) -> (i+1,j+1, l)) (allSubLists xs)

main :: IO ()
main = do
    print (map (\(i,j,l) -> (i,j,l, sum l)) (allSubLists [3, -4, 2, 1]))
