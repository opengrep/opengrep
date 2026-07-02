include("impl.jl")

function source()
    ENV["SECRET"]
end

tainted = source()
greet(tainted)
