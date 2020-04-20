module Main where

import           Parser

main :: IO ()
-- main = print (spaces "     1abc")
-- main = print (chars 5 "1234567")
-- main = print ((require "abc") "abc123")
-- main = print ((spaces #- digit) "   122")
-- main = print ((spaces # digit) "   122")
-- main = print (((spaces # digit) #> (\(a, b) -> Parser.return a)) "   122")
-- main = print ((spaces # spaces) "   122")
main = print (require "abc" "abc123")
