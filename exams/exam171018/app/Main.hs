module Main where
import           Prelude                 hiding ( tail
                                                , init
                                                , map
                                                , filter
                                                )

tail :: [a] -> [a]
tail []           = []
tail [x         ] = []
tail (x : y : xs) = y : xs

init :: [a] -> [a]
init []       = []
init [x     ] = []
init (x : xs) = x : init xs

suffixes :: [a] -> [[a]]
suffixes []       = []
suffixes (x : xs) = (x : xs) : suffixes xs

prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes xs = xs : prefixes (init xs)

data Tree a = Leaf a | Node (Tree a) (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) (Node (Node (Leaf 2) (Leaf 3)) (Leaf 4)))
         (Node (Leaf 5) (Node (Leaf 6) (Node (Leaf 7) (Leaf 8))))

sumTree :: Tree Int -> Int
sumTree (Leaf n  ) = n
sumTree (Node l r) = sumTree l + sumTree r

map :: (a -> b) -> [a] -> [b]
map f xs = [ f x | x <- xs ]

filter :: (a -> Bool) -> [a] -> [a]
filter f xs = [ x | x <- xs, f x ]

evenCubeSum :: Int -> Int
evenCubeSum n =
    foldr (+) 0 (map (\x -> x * x * x) (filter (\x -> x `mod` 2 == 0) [1 .. n]))


main :: IO ()
main = do
    print $ head [1, 2, 3]
    print $ tail [1, 2, 3]
    print $ init [1, 2, 3]
    print $ suffixes [1, 2, 3]
    print $ prefixes [1, 2, 3]
    print $ sumTree t
    print $ map (* 2) [1 .. 9]
    print $ filter (\x -> x `mod` 2 == 0) [1 .. 9]
    print $ evenCubeSum 5
