# Python [a or b or c] lowers to one n-ary [__op_||__] call, which the
# partial evaluator cannot fold (it folds binary And/Or only). As a DNF
# guard the chain contributes one clause per disjunct, so a call that
# refutes every disjunct drops the effect; previously the whole chain was
# a single opaque atom and the refuted call still reported.


