include("mid.jl")
include("impl.jl")

function source()
    ENV["SECRET"]
end

x = source()
y = process(x)
emit(y)
