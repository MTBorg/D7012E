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
import           Debug.Trace
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Write String |
    While Expr.T Statement |
    Begin [Statement] |
    Read String |
    Skip |
    Repeat Statement Expr.T
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
repeat =
    accept "repeat"
        -#  statement
        #-  require "until"
        #   Expr.parse
        #-  require ";"
        >-> buildRepeat
    where buildRepeat (stmts, e) = Repeat stmts e

statement =
    assignment
        ! write
        ! if_
        ! skip
        ! while
        ! begin
        ! Statement.read
        ! Statement.repeat

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
exec (Repeat s cond : stmts) dict input = exec (s : condStmt : stmts)
                                               dict
                                               input
    where condStmt = If cond Skip (Repeat s cond)
exec [] _ _ = []

indent :: Int -> String
indent i = replicate i '\t'

printStmtBlock :: Int -> [Statement.T] -> String
printStmtBlock ind (s : stmts) =
    "\n" ++ shw (ind + 1) s ++ printStmtBlock ind stmts
printStmtBlock ind [] = ""

-- Takes an indentation level and a statement to print. The indentation level
-- rules how many tabs will be printed.
shw :: Int -> Statement -> String
shw ind (Assignment v e) = indent ind ++ v ++ " := " ++ toString e
shw ind (If cond thenStmt elseStmt) =
    indent ind
        ++ "if "
        ++ toString cond
        ++ " then\n"
        ++ shw (ind + 1) thenStmt
        ++ "\n"
        ++ indent ind
        ++ "else\n"
        ++ shw (ind + 1) elseStmt
shw ind (While cond stmt) =
    "while " ++ toString cond ++ "\n" ++ shw (ind + 1) stmt
shw ind (Begin stmts) =
    indent ind
        ++ "begin"
        ++ printStmtBlock ind stmts
        ++ "\n"
        ++ indent ind
        ++ "end"
shw ind (Write v) = indent ind ++ "write " ++ v ++ ";"
shw ind Skip      = indent ind ++ "skip;"
shw ind (Read v)  = indent ind ++ "read " ++ v ++ ";"
shw ind (Repeat stmts cond) =
    indent ind
        ++ "repeat "
        ++ shw (ind + 1) stmts
        ++ "\nuntil "
        ++ toString cond

instance Parse Statement where
    parse    = statement
    toString = shw 0
