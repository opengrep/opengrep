# Ruby destructuring in block/lambda params: both `{ |(a, _)| ... }` and
# `->((a, _b)) { ... }` produce G.ParamPattern(PatTuple[...]) with no
# resolved implicit-parameter binding, so taint routed through the
# collection or the lambda application cannot bind onto a.

def test_each_destructure
  pairs = [[source(), "y"]]
  pairs.each { |(a, _)|
    # ruleid: test-param-pattern-taint
    sink(a)
  }
end

def test_lambda_destructure
  lam = ->((a, _b)) {
    # ruleid: test-param-pattern-taint
    sink(a)
  }
  lam.call([source(), "y"])
end

# Baseline: plain block parameter (G.Param). Passes today.
def test_each_plain
  xs = [source()]
  xs.each { |a|
    # ruleid: test-param-pattern-taint
    sink(a)
  }
end
