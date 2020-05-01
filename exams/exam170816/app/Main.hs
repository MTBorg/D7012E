module Main where
import           Prelude                 hiding ( concat
                                                , filter
                                                )

append :: [a] -> [a] -> [a]
append []       ys = ys
append (x : xs) ys = x : append xs ys

concat :: [[a]] -> [a]
concat []       = []
concat (x : xs) = append x (concat xs)

data TT a = Leaf | Node (TT a) (TT a) (TT a) a deriving (Show, Eq, Ord)

t :: TT Int
t = Node
    (Node Leaf Leaf Leaf 2)
    (Node (Node Leaf Leaf Leaf 4)
          (Node Leaf Leaf Leaf 5)
          (Node Leaf Leaf Leaf 6)
          3
    )
    (Node Leaf Leaf Leaf 7)
    1

prune :: TT a -> TT a
prune Leaf                    = Leaf
prune (Node Leaf Leaf Leaf _) = Leaf
prune (Node l1   l2   l3   n) = (Node p1 p2 p3 n)  where
    p1 = prune l1
    p2 = prune l2
    p3 = prune l3

maxPrune :: TT a -> Int
maxPrune Leaf              = 0
maxPrune (Node l1 l2 l3 n) = helper 0 (Node l1 l2 l3 n)
  where
    helper n m = case (prune m) of
        (Node Leaf Leaf Leaf _) -> n + 1
        _                       -> helper (n + 1) (prune m)


calculator :: IO ()
calculator = calculatorHelp []
  where
    calculatorHelp :: [Int] -> IO ()
    calculatorHelp xs = do
        input <- getLine
        if input == ""
            then do
                putStr "Numbers entered: "
                print xs
                putStrLn ""
                putStr "Sum "
                print $ foldr (+) 0 xs
            else do
                calculatorHelp (xs ++ [read input :: Int])

filter :: (a -> Bool) -> [a] -> [a]
filter _ []       = []
filter f (x : xs) = if f x then x : (filter f xs) else filter f xs

squareSum :: Int -> Int
squareSum n = foldr (+) 0 $ map (\x -> x*x) [x | x <- [1..n]]

main :: IO ()
-- main = calculator
main = do
    print $ append [1, 2, 3] [4, 5, 6]
    print $ concat [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
    print $ prune t
    print $ maxPrune t
    print $ filter (\x -> x `mod` 2 == 0) [1, 2, 3, 4, 5, 6, 7, 8, 9]
    print $ squareSum 4
