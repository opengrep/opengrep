include("a.jl")
include("b.jl")

function source()
    ENV["SECRET"]
end

t = source()
handle(t)
