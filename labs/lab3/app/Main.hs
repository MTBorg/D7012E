-- Martin Terneborg

module Main where

import           Program
import           TestProgram

-- p :: Program.T
-- p =
--     Program.fromString
--         "\
-- \ read n; \
-- \ s:=0; \
-- \ repeat \
-- \     begin \
-- \         s:=s+n; \
-- \         n:=n-1; \
-- \     end \
-- \ until (0-n)+1; \
-- \ write s; "

pr :: Program.T
pr =
    fromString
        "\
\read k;\
\write k;\
\repeat\
\ begin\
\ k := k + 1;\
\ write k;\
\ end\
\until k;"


spr = putStr (toString pr)
rpr k = Program.exec pr [k]

main :: IO ()
main = do
    -- putStrLn $ toString p
    -- print $ exec p [5]

    putStrLn "\nrpr (-2)"
    print $ rpr (-2)

    putStrLn "\nrpr (-1)"
    print $ rpr (-1)

    putStrLn "\nrpr 0"
    print $ rpr 0

    putStrLn "\nrpr 1"
    print $ rpr 1

    putStrLn "\nrpr 2"
    print $ rpr 2

    putStrLn "\nrp"
    print rp

    putStrLn "\nrp1"
    print rp1
