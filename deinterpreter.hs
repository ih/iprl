--get rid of AbstractExpr

data Assignment = Assign Label Expr
type Assignments = [Assignment]

substitute :: Assignments -> Expr -> Expr
substitute a e = map (replace a) (components e)

type SubExpr = [Expr]
data Expr = Expr Name Label SubExprs 

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

deapply :: Rule -> Expr -> Expr
deapply r e = substitute fixedTerms (antecedent Rule)
    where fixedTerms = match (consequent r) e 

applicable :: Expr -> Rule -> Bool
applicable e r = (match e (consequent r)) /= []

oneBack :: Semantics -> Expr -> [Expr]
oneBack s e = map deapply (filter (applicable e) s)

replace :: Expr -> SubExpr -> Expr -> Expr
replace e s replacement = 

replaceSubExpr :: Expr -> SubExpr -> SubExpr -> Expr
replaceSubExpr expr target replacement = undefined

type Depth = Int

oneBackLst :: Semantics -> [Expr] -> [Expr]
oneBackLst s exprLst = concat (map oneBack exprLst)

deval :: Depth -> Semantics -> Expr -> [Expr]
deval s 0 expr = [expr]
deval s n expr = concat map replaceDevalSubExpr (getSubExprs expr)
    where replaceDevalSubExpr subExpr = map (replaceSubExpr expr subExpr) (deval (n-1) (oneBack s (getExpr subExpr)))
