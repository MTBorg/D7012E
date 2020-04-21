module Main where

import           Dictionary
import           Statement

d :: Dictionary.T String Integer
d = Dictionary.insert ("a", 9) Dictionary.empty

-- Should print 11, 13 and whatever you pass to it + 5
whileLoop :: [Statement.T]
whileLoop =
    [ Statement.fromString "read n;"
    , Statement.fromString "a:=2;"
    , Statement.fromString "b:=5;"
    , Statement.fromString "while 10 - a do begin a := a+1; b:=b+1; end"
    , Statement.fromString "if a then skip; else a := a + 10;"
    , Statement.fromString "a := a + 1;"
    , Statement.fromString "n := n + 5;"
    , Statement.fromString "write a;"
    , Statement.fromString "write b;"
    , Statement.fromString "write n;"
    ]

main :: IO ()
main = print $ exec whileLoop d [12]
