# The same callee sink effect instantiated at two call sites of one
# caller produces identity-equal items (the sink trace is not part of
# item identity) whose instantiated guards differ: [3 == 2] from the
# first call and [2 == 2] from the second. Fusing the recordings ORs the
# item guards; both recordings carry top effect-level guards, so the
# insertion no-op check must see the item-guard refinement
# ([Effect.guards_equal], not just effect-level guards). Otherwise the
# first-recorded [3 == 2] is kept alone, folds false at match time, and
# the finding below -- which flag-off reports -- is lost.


