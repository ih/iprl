data Assignment = Assign Label Expr
type Assignments = [Assignment]

substitute :: Assignments -> Expr -> Expr
substitute a e = map (replace a) (components e)

data AbstractExpr = AbstractExpr Expr Labels

match :: AbstractExpr -> Expr -> Assignments
match c e 
    | Unknown == expr c = [Assign (head . labels c) e]
    | otherwise = error "Current system does not take into account non-Unknown consequents"

type Antecedent = AbstractExpr
type Consequent = AbstractExpr
data Rule = Rule {
      antecedent :: Antecedent , 
      consequent :: Consequent ,
    } deriving (Show, Eq)

type Semantics = [Rule]

deapply :: Rule -> Expr
deapply r e = substitute fixedTerms (antecedent Rule)
    where fixedTerms = match (consequent r) e 

applicable :: Expr -> Rule -> Bool
applicable e r = (match e (consequent r)) /= []

oneBack :: Semantics -> Expr -> [Expr]
oneBack s e = map deapply (filter (applicable e) s)

type Depth = Int
deval :: Depth -> Expr -> Hypotheses 
deval n expr = concat.(map deval (n-1)) newExprs
    where newExprs = oneBack semantics (subExprs expr)

deval n expr = map ((map . replace expr) . deval (n-1)) (subExprs expr)
    where replace = deval (n-1) 

