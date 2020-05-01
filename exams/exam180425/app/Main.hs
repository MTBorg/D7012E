module Main where

isOrdered :: Ord a => [a] -> Bool
isOrdered []           = True
isOrdered [x         ] = True
isOrdered (x : y : xs) = x <= y && isOrdered xs

mkList :: Int -> Int -> [Int]
mkList f s = f : mkList (f + (s - 1)) s

data Tree a = Leaf | Node (a,Int) (Tree a) (Tree a) (Tree a)

extract :: Int -> Tree a -> [a]
extract _ Leaf                       = []
extract i (Node (fst, snd) s1 s2 s3) = if i == snd then fst : subs else subs
    where subs = extract i s1 ++ extract i s2 ++ extract i s3

t :: Tree Char
t = Node
    ('X', 1)
    (Node ('H', 2) Leaf Leaf Leaf)
    Leaf
    (Node ('Z', 4)
          (Node ('Y', 3) Leaf Leaf Leaf)
          (Node ('J', 2) Leaf Leaf Leaf)
          Leaf
    )

getRnd = 5

game :: IO ()
game = do
    putStrLn "welcome to the game"
    helper 1 getRnd
  where
    helper n answer = do
        putStrLn $ "Enter guess number " ++ show n
        input <- getInt
        if input == answer
            then putStrLn $ "Correct after " ++ show n ++ " guesses"
            else do
                if input < answer
                    then putStrLn "Too low"
                    else putStrLn "Too high"
                helper (n + 1) answer
        where getInt = read <$> getLine

main :: IO ()
main = game
-- main = do
--     print $ take 5 (mkList 1 (-4))
--     print $ take 5 (mkList 1 1)
--     print $ take 5 (mkList 1 4)
--     print $ extract 2 t
