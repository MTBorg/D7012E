module Main where

import           Prelude                 hiding ( concatMap )

primes = [1, 2, 3, 5, 7, 11, 13]

goldBach :: Int -> Bool
goldBach n = length [ x | x <- primes, y <- primes, x + y == n ] > 0
-- goldBach n = 
--     where helper n

periodise :: [a] -> [a]
periodise [] = []
periodise xs = xs ++ reverse xs ++ periodise xs

data BT a = Leaf a | Node [BT a]

mapBT :: (a -> b) -> BT a -> BT b
mapBT f (Leaf a ) = Leaf (f a)
mapBT f (Node xs) = Node (map (mapBT f) xs)

concatMap f = foldr (\x acc -> (f x) : acc) []
-- concatMap f = foldr ((++) . f)

main :: IO ()
main = do
    print $ take 20 $ periodise [1, 2, 3, 4, 5]
    -- print $ (concatMap (* 2) [[1, 2, 3], [4, 5, 6], [7, 8, 9]])
    print $ goldBach 20
