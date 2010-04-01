import qualified Data.Map as Map
import Data.List (foldl')
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






-- replace :: Expr -> SubExpr -> Expr -> Expr
-- replace e s replacement = 
type Name = String
type Label = String
data SubExpr = SubExpr {
      getLabel :: Label,
      getExpr :: Expr
    } deriving Show


data Expr = Expr Name [SubExpr]
          | Component SubExpr
          | Unknown
            deriving Show
-- data Expr = SubExpr Label Expr
--           | TExpr Name [SubExpr]
--           | Unknown

getSubExprs :: Expr -> [SubExpr]
getSubExprs (Expr _ subExprs) = subExprs
getSubExprs (Component s) = [s]
getSubExprs _ = []

type Semantics = [Rule]
rules :: Semantics -> [Rule]
rules s = s
type Antecedent = Expr
type Consequent = Expr

data Rule = Rule {
      antecedent :: Antecedent, 
      consequent :: Consequent
    } deriving Show

replaceSubExpr :: Expr -> SubExpr -> Expr
replaceSubExpr expr replacement = undefined


--match should pattern match on Expr (Expr SubExpr)
--match is non-commutative i.e. match a b /=> match b a, it takes the unknown subexpressions in the first argument and matches them to either subexpressions in the second or if the first is a single subexpression it matches the 
bindUnknown :: Expr -> Expr -> [SubExpr]
bindUnknown (Component c) e = undefined
bindUnknown _ _ = undefined

--for now target is same as replacement
--if a rule is not applicable it returns an empty 
deapply :: Expr -> Rule -> Expr
deapply e r = foldl' replaceSubExpr e assignments
    where assignments = bindUnknown (consequent r) e 

oneBack :: Semantics -> Expr -> [Expr]
oneBack s e = map (deapply e) (rules s)

type Depth = Int

deval :: Semantics -> Depth -> Expr -> [Expr]
deval s 0 expr = [expr]
deval s n expr = concat (map replaceDevalSubExpr (getSubExprs expr))
    where replaceDevalSubExpr subExpr = map (replaceSubExpr expr.SubExpr (getLabel subExpr)) (concat (map (deval s (n-1)) (oneBack s (getExpr subExpr))))
