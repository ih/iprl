--example of match

import qualified Data.Map as Map

------------------
data TC = TC
    { getA :: Int,
      getB :: String
    } deriving Show

d :: TC -> Int
d (TC a b) = a+5 

data Test = Test Int Mest
          | Unknown
            deriving Show
data Mest = Mest Int Test
            deriving Show
m = Mest 3 Unknown
t = Test 4 m
t2 = Test 5 (Mest 6 t)

apply f args = foldr f [] args

matches lst elm = elm `elem` lst

data Mtype = Mtype
           | B String
           | C Int 
             deriving Show

f :: [Mtype] -> Mtype
f ms = head ms