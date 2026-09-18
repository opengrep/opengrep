function Foo.bar(x)
    # ruleid: taint_julia_dotted_definition_call_in_function
    sink(x)
end
function g()
    Foo.bar(source())
end
g()
