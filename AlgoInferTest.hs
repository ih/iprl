import AlgoInfer

e = Episode [(True, push),(True, push),(False, none)]

infer e

#binary input
light = Expr "BVar"

#binary action
push = Command "Push" []