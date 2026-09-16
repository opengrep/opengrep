function Foo.bar(x)
    # ruleid: taint-julia-dotted-definition
    sink(x)
end
Foo.bar(source())
