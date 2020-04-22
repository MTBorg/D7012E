module Statement
    ( T
    , parse
    , toString
    , fromString
    , exec
    )
where
import           Prelude                 hiding ( return
                                                , fail
                                                )
import           Parser                  hiding ( T )
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Write String |
    While Expr.T Statement |
    Begin [Statement] |
    Read String |
    Skip
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
    where buildAss (v, e) = Assignment v e
write = accept "write" -# word #- require ";" >-> Write
skip = accept "skip;" >-> const Skip
while = accept "while" -# Expr.parse #- require "do" # statement >-> buildWhile
    where buildWhile (e, s) = While e s
begin = accept "begin" -# iter statement #- require "end" >-> Begin
 -- TODO: This should reject empty then/else statements
if_ =
    accept "if "
        -#  Expr.parse
        #-  require "then"
        #   statement
        #-  require "else"
        #   statement
        >-> buildIf
    where buildIf ((cond, ifStmt), elseStmt) = If cond ifStmt elseStmt
read = accept "read" -# word #- require ";" >-> Read

statement = assignment ! write ! if_ ! skip ! while ! begin ! Statement.read

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]
exec (Assignment v e : stmts) dict input = exec stmts newDict input
    where newDict = Dictionary.insert (v, Expr.value e dict) dict
exec (If cond thenStmts elseStmts : stmts) dict input =
    if Expr.value cond dict > 0
        then exec (thenStmts : stmts) dict input
        else exec (elseStmts : stmts) dict input
exec (Write v : stmts) dict input = case Dictionary.lookup v dict of
    Just n  -> n : exec stmts dict input
    Nothing -> error $ "Could not find variable " ++ v
exec (Skip         : stmts) dict input = exec stmts dict input
exec (While cond s : stmts) dict input = if Expr.value cond dict > 0
    then exec (s : (While cond s : stmts)) dict input
    else exec stmts dict input
exec (Begin xs : stmts) dict input = exec (xs ++ stmts) dict input
exec (Read  v  : stmts) _    []    = error "Missing input"
exec (Read v : stmts) dict (i : ip) =
    exec stmts (Dictionary.insert (v, i) dict) ip
exec [] _ _ = []

shw :: Statement -> String
shw (Assignment v e) = v ++ " := " ++ toString e
shw (If cond thenStmt elseStmt) =
    "if "
        ++ toString cond
        ++ " then\n\t"
        ++ toString thenStmt
        ++ "\nelse\n\t"
        ++ toString elseStmt
shw (While cond stmt) = "while " ++ toString cond ++ "\n\t" ++ toString stmt
shw (Begin stmts    ) = "begin" ++ printStmts stmts ++ "\n\tend"
  where
    printStmts (s : stmts) = "\n\t" ++ shw s ++ printStmts stmts
    printStmts []          = ""
shw (Write v) = "write " ++ v ++ ";"
shw Skip      = "skip;"
shw (Read v)  = "read " ++ v ++ ";"

instance Parse Statement where
    parse    = statement
    toString = shw
