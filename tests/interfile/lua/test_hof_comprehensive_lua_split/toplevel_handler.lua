function toplevel_handler(x)
    -- ruleid: test-hof-taint
    sink(x)
    return x
end
