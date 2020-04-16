module Main where

import           Expr

main :: IO ()
main = print (unparse (simplify (diff (Var "x") (parse "exp(sin(2*x))"))))
