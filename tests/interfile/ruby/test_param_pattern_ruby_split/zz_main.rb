require_relative 'test_each_destructure'
require_relative 'test_lambda_destructure'
require_relative 'test_each_plain'
# Ruby destructuring in block/lambda params: both `{ |(a, _)| ... }` and
# `->((a, _b)) { ... }` produce G.ParamPattern(PatTuple[...]) with no
# resolved implicit-parameter binding, so taint routed through the
# collection or the lambda application cannot bind onto a.



# Baseline: plain block parameter (G.Param). Passes today.
