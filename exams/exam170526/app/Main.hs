module Main where

prefix _ [] = []
prefix n (x:xs) =
    if n<=0
      then []
      else x : prefix (n-1) xs

suffix n xs = reverse (prefix n (reverse xs))

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving(Show, Eq, Ord)

t :: Tree Int
t = Node (Node (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))) (Leaf 4)) (Node (Leaf 5) (Leaf 6))

flatten :: Tree a -> [a]
flatten (Leaf n) = [n]
flatten (Node l r) = flatten l ++ flatten r

balance :: Tree a -> Tree a
balance (Leaf n) = Leaf n
balance (Node l r) = balanceRec flattened
    where 
        flattened = flatten (Node l r)
        balanceRec :: [a] -> Tree a
        balanceRec (x:xs)
          | xs == [] = Leaf x
          | otherwise = Node (balanceRec (prefix (Prelude.div len 2) xs)) (balanceRec (suffix ((Prelude.div len 2)-1) xs) )
        len = length flattened

cashReg :: IO ()
cashReg = cashRegIt 0
  where
    cashRegIt s = do
        input <- getLine
        if input == ""
            then do
                print s
                putStrLn "-----"
                cashRegIt 0
            else if input == "e"
                then do
                    return ()
                else cashRegIt (s + (read input :: Int))

myMap f = foldr (\x acc -> (f x) : acc) []

main :: IO ()
-- main = cashReg
main = do
    -- print $ myMap (\x -> x + 2) [1, 2, 3, 4, 5, 6]
    print $ balance t
