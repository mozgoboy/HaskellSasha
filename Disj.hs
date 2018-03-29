data Literal = Var Int
             | Not Int
             deriving (Show,Read,Eq)
type Disj = [Literal]
type Array = [Disj]
f :: Disj -> Disj -> Disj
f [] b = b
f _ [] = []
f [a] (b:bs) | a == snot b = bs
             | bs==[] =[b]
             | otherwise = [b] ++ (f [a] bs)
f (a:as) (b:bs) | f [a] (b:bs) /= (b:bs) =  as ++ (f [a] (b:bs))
                | test (a:as) (b:bs) ==False = (b:bs)
                | otherwise =  a : f as (b : bs)

simplify::Disj->Disj
simplify [] = []
simplify [a] = [a]
simplify (a:as) | test [snot a] as == True = simplify as
                | otherwise = a : simplify as



test::Disj->Disj->Bool
test [] _ = False
test _ [] = False
test [a] (b:bs) | a == snot b = True
                | bs== [] = False
                | otherwise = test [a] bs
test (a:as) (b:bs) | test [a] (b:bs) = True
                   | otherwise = test as (b:bs)


snot :: Literal->Literal
snot (Var x) = (Not x)
snot (Not x) = (Var x)
first :: Disj->Disj
first (a:as) = [a]
remove :: Disj->Disj
remove (a:as) = as
-- gosha comment -- 
