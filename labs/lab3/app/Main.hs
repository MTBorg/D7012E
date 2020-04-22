module Main where

import           Program

-- Should print 11, 13 and whatever you pass to it + 5
p :: Program.T
p =
    Program.fromString
        "\ 
    \ read n; \
    \ a:=2; \
    \ b:=5; \
    \ while 10 - a do begin a := a+1; b:=b+1; end \
    \ if a then skip; else a := a + 10; \
    \ a := a + 1; \
    \ n := n + 5; \
    \ write a; \
    \ write b; \
    \ write n; \
\ "

main :: IO ()
main = print $ Program.exec p [18]
