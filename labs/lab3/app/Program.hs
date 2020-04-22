module Program
    ( T
    , parse
    , fromString
    , toString
    , exec
    )
where
import           Parser                  hiding ( T )
import qualified Statement
import qualified Dictionary
import           Prelude                 hiding ( return
                                                , fail
                                                )
newtype T = Program [Statement.T] -- to be defined

program :: Parser T
program = iter (spaces -# Statement.parse #- spaces) >-> Program

shw :: T -> String
shw (Program (s : stmts)) = (toString s) ++ "\n" ++ shw (Program stmts)
shw (Program []         ) = ""


instance Parse T where
    parse    = program
    toString = shw

exec :: Program.T -> [Integer] -> [Integer]
exec (Program stmts) = Statement.exec stmts Dictionary.empty
