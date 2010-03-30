data Test = Test Int
          | Mest Int Int
            deriving Show

apply f args = foldr f [] args

matches lst elm = elm `elem` lst

data Mtype = Mtype
           | B String
           | C Int 
             deriving Show

f :: [Mtype] -> Mtype
f ms = head ms