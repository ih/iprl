data Exp = T
         | F
         | If Exp Exp Exp
         | Unknown
           deriving Show

eval (T) = T
eval (F) = F
eval (If (T) e1 e2) = eval e1
eval (If (F) e1 e2) = eval e2
eval (If e0 e1 e2) = eval (If (eval e0) e1 e2)



-- type Depth = Int
-- deval :: Depth -> Exp -> Hypotheses 
-- deval n exp = concat.(map deval (n-1)) newExprs
--     where newExprs = oneStep exp semantics

deval 1 exp = 
                  
deval _ (T) = [T]
deval _ (F) = [F]
deval n (If e0 e1 e2) = combine If (deval (n-1) e0) (deval (n-1) e1) (deval (n-1) e2)

deval 0 exp = [exp]
--need to write a function that takes a syntactic form, a variable within that expression, and a list of expressions and generates a list of the syntactic form with each expression in the list substituted into the variable
--deval n e = (If (T) (deval (n-1) e) (Unknown))):