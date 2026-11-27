function f(x, y)
    handler = z -> sink(z)  # ok: local-lambdas-of-two-functions-not-merged
    handler(y)
end
