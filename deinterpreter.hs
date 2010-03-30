import qualified Data.Map as Map

-- --get rid of AbstractExpr

-- data Assignment = Assign Label Expr
-- type Assignments = [Assignment]

-- substitute :: Assignments -> Expr -> Expr
-- substitute a e = map (replace a) (components e)

-- type SubExpr = [Expr]
-- data Expr = Expr Name Label SubExprs 

-- data AbstractExpr = AbstractExpr Expr Labels


-- type Antecedent = AbstractExpr
-- type Consequent = AbstractExpr
-- data Rule = Rule {
--       antecedent :: Antecedent , 
--       consequent :: Consequent ,
--     } deriving (Show, Eq)

-- type Semantics = [Rule]




-- replace :: Expr -> SubExpr -> Expr -> Expr
-- replace e s replacement = 
data Expr = Expr Name Map Label Expr
          | SubExpr Label Expr
          | Unknown

applicableRules :: Semantics -> Expr -> [Rule]
applicableRules s e = undefined

replaceSubExpr :: Expr -> SubExpr -> SubExpr -> Expr
replaceSubExpr expr target replacement = undefined

getSubExprs :: Expr -> [SubExpr]
getSubExprs expr = undefined

--match should pattern match on Expr (Expr SubExpr)
match :: Expr -> Expr -> [SubExpr]
match c e 
    | Unknown == expr c = [Assign (head . labels c) e]
    | otherwise = error "Current system does not take into account non-Unknown consequents"
--for now target is same as replacement
deapply :: Rule -> Expr -> Expr
deapply r e = foldr (map (replaceSubExpr (antecedent r)) assignments) assignments
    where assignments = match (consequent r) e 

oneBack :: Semantics -> Expr -> [Expr]
oneBack s e = map deapply (applicableRules s e)

type Depth = Int

deval :: Depth -> Semantics -> Expr -> [Expr]
deval s 0 expr = [expr]
deval s n expr = concat map replaceDevalSubExpr (getSubExprs expr)
    where replaceDevalSubExpr subExpr = map (replaceSubExpr expr subExpr) (deval (n-1) (oneBack s (getExpr subExpr)))
