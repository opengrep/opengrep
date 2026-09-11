include("mid.jl")

function source()
    ENV["SECRET"]
end

t = source()
relay(t)
