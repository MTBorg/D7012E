import           Prelude                 hiding ( sum )

sum = foldr (+) 0

subL :: [Int] -> [(Int, Int, [Int])]
subL (x : xs)
    | null xs   = [(0, 0, [x])]
    | otherwise = (0, 0, [x]) : map (\(i, j, l) -> (i, j + 1, x : l)) (subL xs)

allSubLists :: [Int] -> [(Int, Int, [Int])]
allSubLists [] = []
allSubLists (x : xs)
    | null xs = [(0, 0, [x])]
    | otherwise =  subL (x : xs)
    ++ map (\(i, j, l) -> (i + 1, j + 1, l)) (allSubLists xs)

toStrings :: [(Int, Int, [Int], Int)] -> [String]
toStrings = map
    (\(i, j, l, s) ->
        show s ++ "\t" ++ show i ++ "\t" ++ show j ++ "\t" ++ show l ++ "\n"
    )

sortSets :: [(Int, Int, [Int], Int)] -> [(Int, Int, [Int], Int)]
sortSets []       = []
sortSets (x : xs) = insert x (sortSets xs)
  where
    insert (_, _, _, s) (x2 : x2s) | s < s2    = x : x2 : x2s
                                   | otherwise = x2 : insert x x2s
        where (_, _, _, s2) = x2
    insert x [] = [x]

smallestKSets :: Int -> [Int] -> String
smallestKSets k [] = error "Empty list given an as argument"
smallestKSets k xs = header ++ foldr (++) "" (toStrings (take k sortedSets))
  where
    header     = "size\ti\tj\tsublist\n"
    sortedSets = sortSets
        (map (\(i, j, list) -> (i, j, list, sum list)) (allSubLists xs))

testCase1Set = [ x * (-1) ^ x | x <- [1 .. 100] ]
testCase1k = 15
testCase2Set = [24, -11, -34, 42, -24, 7, -19, 21]
testCase2k = 6
testCase3Set = [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3]
testCase3k = 8

main :: IO ()
-- main = putStrLn (smallestKSets 5 [3, -4, 2, 1])
-- main = putStrLn (smallestKSets testCase1k testCase1Set)
-- main = putStrLn (smallestKSets testCase2k testCase2Set)
-- main = putStrLn (smallestKSets testCase3k testCase3Set)
main = putStrLn (smallestKSets 4 [])
