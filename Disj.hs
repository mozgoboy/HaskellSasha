data Literal = Var Int
             | Not Int
             deriving (Show,Read,Eq)
type Disj = [Literal]
f :: Disj -> Disj -> Disj
f [] _ = []
f _ [] = []
f [a] (b:bs) | a == snot b = bs
             | bs==[] =[b]
             | otherwise = [b] ++ (f [a] bs)
f (a:as) (b:bs) | f [a] (b:bs) == (b:bs) = a : f as (b : bs)
                | otherwise =  as ++ (f [a] (b:bs))




test::Disj->Disj->Bool
test [] _ = False
test _ [] = False
test [a] (b:bs) | a == snot b = True
                | bs== [] = False
                | otherwise = test [a] bs

snot :: Literal->Literal
snot (Var x) = (Not x)
snot (Not x) = (Var x)
first :: Disj->Disj
first (a:as) = [a]
remove :: Disj->Disj
remove (a:as) = as
