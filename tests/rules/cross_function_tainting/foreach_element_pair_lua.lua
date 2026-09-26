function each_value(t, cb)
    for _, v in ipairs(t) do
        cb(v)
    end
end

function tainted_value()
    each_value({source()}, function(v)
        -- ruleid: foreach_element_pair_lua
        sink(v)
    end)
end

function clean_value()
    each_value({"x"}, function(v)
        -- ok: foreach_element_pair_lua
        sink(v)
    end)
end
