-- Ex. 3.7
threeDifferent1 :: Int -> Int -> Int -> Bool
threeDifferent1 m n p | m == n && n == p = False
                      | otherwise        = True

threeDifferent2 :: Int -> Int -> Int -> Bool
threeDifferent2 m n p = if m == n && n == p then False else True

-- Ex. 3.8
twoEqual :: Int -> Int -> Bool
twoEqual a b = a == b

threeEqual :: Int -> Int -> Int -> Bool
threeEqual a b c = twoEqual a b && twoEqual b c

fourEqual :: Int -> Int -> Int -> Int -> Bool
fourEqual a b c d = threeEqual a b c && threeEqual b c d


main :: IO ()
main = do
  putStrLn "Ex. 3.7"
  print (threeDifferent1 1 2 3)
  print (threeDifferent1 1 1 1)
  print (threeDifferent2 1 2 3)
  print (threeDifferent2 1 1 1)

  putStrLn "Ex. 3.8"
  print (fourEqual 1 1 1 1)
  print (fourEqual 1 2 1 1)
