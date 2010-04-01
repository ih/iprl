import Test.QuickCheck
import Deinterpreter
--replaceSubExpr properties
prop_sameLabel expr subExpr = labels expr == labels (replaceSubExpr expr subExpr)


t=Expr "True" []
f=Expr "False" []
--myIf=Expr "If" [(SubExpr "t1" t, f, f]
aIf = Expr "If" [(SubExpr "t0" Unknown), (SubExpr "t1" Unknown), (SubExpr "t2" Unknown)]
tIf = Expr "If" [(SubExpr "t0" t), (SubExpr "t1" Unknown), (SubExpr "t2" Unknown)]
r0 = Rule tIf (Component (SubExpr "t1" Unknown))
s = [r0]
--example of match



------------------
-- data TC = TC
--     { getA :: Int,
--       getB :: String
--     } deriving Show

-- d :: TC -> Int
-- d (TC a b) = a+5 

-- data Test = Test Int Mest
--           | Unknown
--             deriving Show
-- data Mest = Mest Int Test
--             deriving Show
-- m = Mest 3 Unknown
-- t = Test 4 m
-- t2 = Test 5 (Mest 6 t)

-- apply f args = foldr f [] args

-- matches lst elm = elm `elem` lst

-- data Mtype = Mtype
--            | B String
--            | C Int 
--              deriving Show

-- f :: [Mtype] -> Mtype
-- f ms = head ms