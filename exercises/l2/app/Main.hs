-- Ex. 5.2
orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (a, b, c) | a < b && b < c = (a, b, c)
                      | a < c && c < b = (a, c, b)
                      | b < a && a < c = (b, a, c)
                      | b < c && c < a = (b, c, a)
                      | c < a && a < b = (c, a, b)
                      | c < b && b < a = (c, b, a)

-- Ex. 5.10
divisors n = [ x | x <- [1 .. n], n `mod` x == 0 ]

isPrime :: Int -> Bool
isPrime n = divs == [1, n] || divs == [1] where divs = divisors n

-- Ex. 5.11
matches :: Int -> [Int] -> [Int]
matches k xs = [ x | x <- xs, k == x ]

isElem :: Int -> [Int] -> Bool
isElem k xs = not (null (matches k xs))

-- Ex. 5.18


main :: IO ()
main = do
        -- Ex. 5.2
    -- putStrLn "Ex. 5.2"
    -- print (orderTriple (1, 2, 3))
    -- print (orderTriple (1, 3, 2))
    -- print (orderTriple (2, 1, 3))
    -- print (orderTriple (2, 3, 1))
    -- print (orderTriple (3, 1, 2))
    -- print (orderTriple (3, 2, 1))

    -- putStrLn "Ex. 5.10"
    -- print (isPrime 1)
    -- print (isPrime 2)
    -- print (isPrime 3)
    -- print (isPrime 4)
    -- print (isPrime 5)
    -- print (isPrime 6)
    -- print (isPrime 7)
    -- print (isPrime 8)
    -- print (isPrime 9)
    --
    -- putStrLn "Ex. 5.11"
    -- print (isElem 1 [1, 2, 3, 4, 1, 5, 1])
    -- print (isElem 1 [2, 3, 4, 6])
