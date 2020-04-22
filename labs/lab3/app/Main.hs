module Main where

import           Program

p :: Program.T
p =
    Program.fromString
        "\
\ read n; \
\ s:=0; \
\ repeat \
\     begin \
\         s:=s+n; \
\         n:=n-1; \
\     end \
\ until (0-n)+1; \
\ write s; "

main :: IO ()
main = print $ exec p [5]
