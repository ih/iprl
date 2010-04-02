module Deinterpreter where 

import qualified Data.Map as Map
import Data.List (foldl')

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

getSubLabels :: Expr -> [Label]
getSubLabels (Expr _ subExprs) = map getLabel subExprs
getSubLabels (Component s) = [getLabel s]
getSubLabels _ = []

getSubExprs :: Expr -> [SubExpr]
getSubExprs (Expr n subExprs) = (SubExpr "Self" (Expr n subExprs)):subExprs
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

changeSub :: SubExpr -> SubExpr -> SubExpr
changeSub (SubExpr l2 new) (SubExpr l1 old) 
    | l1 == l2 = (SubExpr l1 new)
    | otherwise = (SubExpr l1 old)

replaceSubExpr :: Expr -> SubExpr -> Expr
replaceSubExpr _ (SubExpr "Self" e) = e
replaceSubExpr (Expr name s) replacement = Expr name (map (changeSub replacement) s)

--match should pattern match on Expr (Expr SubExpr)
--match is non-commutative i.e. match a b /=> match b a, it takes the unknown subexpressions in the first argument and matches them to either subexpressions in the second or if the first is a single subexpression it matches the 
bindUnknown :: Expr -> Expr -> [SubExpr]
bindUnknown (Component (SubExpr l Unknown)) e = [(SubExpr l e)]
bindUnknown _ _ = undefined

--for now target is same as replacement
--if a rule is not applicable it returns an empty 
deapply :: Expr -> Rule -> Expr
deapply e r = foldl' replaceSubExpr (antecedent r) assignments
    where assignments = bindUnknown (consequent r) e 

oneBack :: Semantics -> Expr -> [Expr]
oneBack s e = map (deapply e) (rules s)

type Depth = Int

deval :: Semantics -> Depth -> Expr -> [Expr]
deval s 0 expr = [expr]
deval s n expr = concat (map replaceDevalSubExpr (getSubExprs expr))
    where replaceDevalSubExpr subExpr = map (replaceSubExpr expr.SubExpr (getLabel subExpr)) (concat (map (deval s (n-1)) (oneBack s (getExpr subExpr))))
