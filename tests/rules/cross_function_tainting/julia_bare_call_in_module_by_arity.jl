module M
function f(a)
    # ruleid: bare-call-in-module-by-arity
    sink(a)
end
function f(a, b)
    # ok: bare-call-in-module-by-arity
    sink(b)
end
function g()
    f(source())
end
function h()
    f(source(), "c")
end
end
