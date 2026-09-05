# Two return statements with identical taints but different path guards have
# one effect identity: [Effects] fuses their guards disjunctively on insertion
# ([Effect.compare] ignores guards), so the signature carries a single
# ToReturn effect guarded by [(a == 1) || (!(a == 1) && (a == 2))]. A call
# refuting both disjuncts drops the effect; a call satisfying one keeps it.


