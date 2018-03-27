data Literal = Var Int
             | Not Int
             deriving (Show,Read,Eq)
type Conj = [Literal]
f :: Conj -> Conj -> Conj
f [] _ = []
f _ [] = []
f [a] (b:bs) | a == snot b = bs
             | bs== [] = [b]
             | f [a] bs == remove bs = (b : (remove bs))
             | otherwise = (b:bs)



test::Conj->Conj->Bool
test [] _ = False
test _ [] = False
test [a] (b:bs) | a == snot b = True
                | bs== [] = False
                | otherwise = test [a] bs

snot :: Literal->Literal
snot (Var x) = (Not x)
snot (Not x) = (Var x)
first :: Conj->Conj
first (a:as) = [a]
remove :: Conj->Conj
remove (a:as) = as
hlzhclzxjkch
