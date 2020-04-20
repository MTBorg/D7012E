module Main where

import           Parser
import           Expr
import           Dictionary

d :: Dictionary.T String Integer
d = Dictionary.insert ("a", 3) Dictionary.empty


main :: IO ()
main = do
-- main = print (spaces "     1abc")
-- main = print (chars 5 "1234567")
-- main = print ((require "abc") "abc123")
-- main = print ((spaces #- digit) "   122")
-- main = print ((spaces # digit) "   122")
-- main = print (((spaces # digit) #> (\(a, b) -> Parser.return a)) "   122")
-- main = print ((spaces # spaces) "   122")
-- main = print (require "abc" "abc123")
-- main = print (toString (fromString "2" :: Expr))
-- main = print (value (fromString "2") empty)
    print (value (fromString "a/2") d)
