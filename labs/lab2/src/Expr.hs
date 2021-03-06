-- Code to Haskell lab assignment 2 in the course D7012E by Håkan Jonsson
module Expr where

import           Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
  where
    -- Return whether or not p evaluates to false using the first element of 
    -- a list.
    notfirst p (_, []    ) = True
    notfirst p (_, x : xs) = not (p x)

    -- Build a number by extracting the leading digits from a given string. 
    -- Also returns the remaining string as the second element of the tuple.
    -- E.g. "123abc456 -> (Const "123", "abc456")
    buildnumber :: String -> (EXPR, String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        -- Add the value of the first character in the string to the given
        -- expression value. E.g. (Const 10, "72abc") -> (Const 107, "2abc")
        accdigits :: (EXPR, String) -> (EXPR, String)
        accdigits (Const n, y : ys) = (Const (10 * n + (ord y - 48)), ys)

    -- Build a variable by extracting the leading non-numeric characters from a
    -- given string. Also returns the remaining string as the second element of
    -- the tuple. E.g. "myvar123abc" -> (Var myvar, "123abc")
    buildvar :: String -> (EXPR, String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR, String) -> (EXPR, String)
        accletters (Var s, y : ys) = (Var (s ++ [y]), ys)


    buildexpr :: String -> (EXPR, String)
    buildexpr xs = until (notfirst (\c -> c == '-' || c == '+'))
                         accterms
                         (buildterm xs)
      where
        accterms :: (EXPR, String) -> (EXPR, String)
        accterms (term, y : ys) = (Op (y : []) term term1, zs)
            where (term1, zs) = buildterm ys

    buildterm :: String -> (EXPR, String)
    buildterm xs = until (notfirst (\c -> c == '*' || c == '/'))
                         accfactors
                         (buildfactor xs)
      where
        accfactors :: (EXPR, String) -> (EXPR, String)
        accfactors (fact, y : ys) = (Op (y : []) fact fact1, zs)
            where (fact1, zs) = buildfactor ys

    buildfactor :: String -> (EXPR, String)
    buildfactor []         = error "missing factor"
    buildfactor ('(' : xs) = case buildexpr xs of
        (e, ')' : ws) -> (e, ws)
        _             -> error "missing factor"
    buildfactor (x : xs)
        | isDigit x = buildnumber (x : xs)
        | isLetter x = case buildvar (x : xs) of
            (Var s, '(' : zs) ->
                let (e, ws) = buildfactor ('(' : zs) in (App s e, ws)
            p -> p
        | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n      ) = show n
unparse (Var   s      ) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
unparse (App f e      ) = f ++ "(" ++ unparse e ++ ")"

eval :: EXPR -> [(String, Float)] -> Float
eval (Const n) _   = fromIntegral n
eval (Var   x) env = case lookup x env of
    Just y -> y
    _      -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
eval (App "sin" expr   ) env = sin (eval expr env)
eval (App "cos" expr   ) env = cos (eval expr env)
eval (App "log" expr   ) env = log (eval expr env)
eval (App "exp" expr   ) env = exp (eval expr env)

diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2) | id == id2 = Const 1
                        | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) = Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) = Op
    "/"
    (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2)))
    (Op "*" e2 e2)
diff v (App "sin" e) = Op "*" (diff v e) (App "cos" e)
diff v (App "cos" e) = Op "*" (diff v e) (Op "*" (Const (-1)) (App "sin" e))
diff v (App "log" e) = Op "/" (diff v e) e
diff v (App "exp" e) = Op "*" (diff v e) (App "exp" e)
diff _ _             = error "can not compute the derivative"

simplify :: EXPR -> EXPR
simplify (Const n ) = Const n
simplify (Var   id) = Var id
simplify (Op oper left right) =
    let (lefts, rights) = (simplify left, simplify right)
    in  case (oper, lefts, rights) of
            ("+", e      , Const 0) -> e
            ("+", Const 0, e      ) -> e
            ("*", e      , Const 0) -> Const 0
            ("*", Const 0, e      ) -> Const 0
            ("*", e      , Const 1) -> e
            ("*", Const 1, e      ) -> e
            ("-", e      , Const 0) -> e
            ("/", e      , Const 1) -> e
            ("-", le, re) -> if left == right then Const 0 else Op "-" le re
            (op , le     , re     ) -> Op op le re
simplify (App a e) = App a (simplify e)

mkfun :: (EXPR, EXPR) -> (Float -> Float)
mkfun (body, var) arg = eval body [(unparse var, arg)]

findzero :: String -> String -> Float -> Float
findzero s1 s2 = fz var f f'
  where
    var = parse s1
    f   = mkfun (parse s2, var)
    f'  = mkfun (diff var (parse s2), var)
    fz var f f' x0 | abs (guess - x0) <= 0.0001 = guess
                   | otherwise                  = fz var f f' guess
        where guess = x0 - f x0 / f' x0
