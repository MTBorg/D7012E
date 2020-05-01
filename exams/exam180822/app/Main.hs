module Main where

import Prelude hiding ((.))

(.) :: (b -> c) -> (a -> b) -> (a -> c)
(.) g f = (\e -> g (f e))

t :: Int -> Int 
t = (*2).(\x ->x+1)

data T a = Leaf a | Node (T a) a (T a)

transform :: T a -> (a -> a) -> T a
transform (Leaf n) f = Leaf (f n)
transform (Node l n r) f = Node (transform l f) (f n) (transform r f)

valid :: [[Int]] -> (Int, Int) -> Bool
valid (x:xs) (i,j) = i < length (x:xs) && j < length x 

a :: [[Int]] -> (Int, Int) -> Int 
a xs (i, j) = (xs !! (i-1)) !! (j-1)

m = [[1,2,3],[3,4,5],[7,8,9]]

main :: IO ()
main = do 
    print $ t 4
    print $ a m (2,3)
