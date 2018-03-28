module Disj_S where


data Literal = Var Int
             | Not Int
             deriving (Show,Read,Eq)

type Disj = [Literal]

snot :: Literal -> Literal
snot (Var x) = (Not x)
snot (Not x) = (Var x)

headL :: Disj -> Literal
headL (a:as) = a

removeHead :: Disj -> Disj
removeHead [] = []
removeHead (a:as) = as

f :: Disj -> Disj -> Disj -> Disj
f [] _ buff = buff
f _ [] buff = buff

f [a] (b:bs) buff | a == snot b  =  buff ++ bs
                  | bs == []     =  [b]
                  | otherwise    =  [b] ++ (f [a] bs (buff))

f (a:as) (b:bs) buff | f [a] (b:bs) buff /= (b:bs)  =  buff ++ as ++ f [a] (b:bs) buff
                     | as == []                     =  buff ++ (f as (b:bs) buff)
                     | otherwise                    =  f as (b:bs) (buff ++ [a])


simplify1 :: Disj -> Disj -> Disj
simplify1 [a] buff = buff
simplify1 (a:as) buff | a == headL as  =  simplify1 (a : removeHead as) buff
                      | otherwise      =  simplify1 (a : removeHead as) (buff ++ [headL as])

simplify :: Disj -> Disj -> Disj
simplify [] buff  = buff
simplify [a] buff = buff ++ [a]
simplify (a:as) buff = simplify (simplify1 (a:as) []) (buff ++ [a])


--f [Var 1,Var 2,Var 5,Var 6,Not 6] [Var 3, Var 4,Not 5,Var 7,Var 8] []
--simplify simplify [Var 1, Var 3, Var 1, Var 2, Var 2, Var 1, Not 1, Var 1, Var 4, Var 3] []
