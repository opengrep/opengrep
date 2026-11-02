module M
function f(c)
    # ruleid: bare-call-in-module
    sink(c)
end
g() = f(source())
end

module N
function f(c)
    # ok: bare-call-in-module
    sink(c)
end
end
