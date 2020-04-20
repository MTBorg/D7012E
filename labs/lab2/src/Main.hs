module Main where

import           Expr

main :: IO ()
main = do
    -- print (unparse (simplify (diff (Var "x") (parse "exp(sin(2*x))"))))
    -- print (mkfun (parse "x", Var "x") 5)

    putStrLn "findzero \"x\" \"x*x*x+x-1\""
    print (findzero "x" "x*x*x+x-1" 1.0)
    putStrLn ""

    putStrLn "findzero \"y\" \"cos(y)*sin(y)\" 2.0"
    print (findzero "y" "cos(y)*sin(y)" 2.0)
    putStrLn ""
