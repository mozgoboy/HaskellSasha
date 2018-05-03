module Disj_S where


data Literal = Var Int
             | Not Int
             deriving (Show,Read,Eq)

type Disj = [Literal]

snot :: Literal -> Literal
snot (Var x) = (Not x)
snot (Not x) = (Var x)

resolution :: Disj -> Disj -> Disj
resolution as bs = simplify (f as bs []) where
        f :: Disj -> Disj -> Disj -> Disj
        f []     _      buff = buff
        f _      []     buff = buff
        f [a]    (b:bs) buff | a == snot b  = buff ++ bs
                             | bs == []     = [b]
                             | otherwise    = [b] ++ (f [a] bs (buff))

        f (a:as) (b:bs) buff | f [a] (b:bs) buff /= (b:bs)  = buff ++ as ++ f [a] (b:bs) buff
                             | as == []                     = buff ++ (f as (b:bs) buff)
                             | otherwise                    = f as (b:bs) (buff ++ [a])


simplify :: Disj -> Disj
simplify [] = []
simplify (x:xs) | x `elem` xs  = simplify xs
                | otherwise    = x : simplify xs


proof :: [Disj] -> Bool
proof as | fst (fullPass as [])        = True
         | snd (fullPass as []) == []  = False
         | otherwise                   = proof (as ++ snd (fullPass as []))
         where
          partPass :: Disj -> [Disj] -> [Disj] -> (Bool, [Disj])
          partPass [] _     buff = (True, buff)
          partPass _ []     buff = (False, buff)
          partPass a (b:bs) buff | resolution a b == []           = (True, buff)
                                 | (resolution a b) `elem` (b:bs) = partPass a bs buff
                                 | otherwise                      = partPass a bs (resolution a b : buff)
          fullPass :: [Disj] -> [Disj] -> (Bool, [Disj])
          fullPass   []     buff = (False, buff)
          fullPass   (a:as) buff | fst (partPass a as [])         = (True, buff)
                                 | otherwise                      = fullPass as (buff ++ snd (partPass a as []))



--resolution [Var 1,Var 2,Var 5,Var 6,Not 6] [Var 3, Var 4,Not 5,Var 7,Var 8]
