import Deinterpreter
import WeightedCollection

algoInfer :: Semantics -> Experience -> HypothesisSet -> ActionSequence -> HypothesisSet
algoInfer s exps h [] = h
algoInfer s exps h (e:es) = algoInfer exps (combine h h') es
    where h' = deval s (assignWeight e)



--batch inference (generate a hypothesis set for each episode then combine)
batchInfer :: Semantics -> Episodes -> WeightedCollection
batchInfer s es = combine hypotheses es
    where hypotheses = map (deval s . WeightedExpression)  es
combine :: WeightedCollection -> episodes
combine hs es = generateWeightedCollection (id (reweightByAccuracy hs))

reweightByAccuracy :: Episodes -> WeightedExpressions -> WeightedCollection
reweightByAccuracy es wes = map percentEvalCorrect es  

--online inference (generate a single hypothesis set and modify online)
onlineInfer :: Semantics -> Episodes -> WeightedCollection
onlineInfer s es hypotheses = es 

--abstract :: Semantics -> Episodes -> [Expr] 