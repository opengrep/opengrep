function one()
    return source()
end

function two()
    return source(), "x"
end

function clean_first()
    return "x", source()
end

function single_result()
    local h = one()
    -- ruleid: lua_call_results
    sink(h)
end

function two_results()
    local a, b = two()
    -- ruleid: lua_call_results
    sink(a)
    -- ok: lua_call_results
    sink(b)
end

function two_results_assigned()
    local a, b
    a, b = clean_first()
    -- ok: lua_call_results
    sink(a)
    -- ruleid: lua_call_results
    sink(b)
end

function consume(v, w)
    -- ruleid: lua_call_results
    sink(v)
    -- ok: lua_call_results
    sink(w)
end

function non_last_argument()
    consume(one(), "x")
end

function first_of_several()
    local h = clean_first()
    -- ok: lua_call_results
    sink(h)
end

function first_of_several_tainted()
    local h = two()
    -- ruleid: lua_call_results
    sink(h)
end

function first_of_several_assigned()
    local h
    h = clean_first()
    -- ok: lua_call_results
    sink(h)
end

function parenthesised_call()
    local h = (clean_first())
    -- ok: lua_call_results
    sink(h)
end

function parenthesised_call_to_two_targets()
    local a, b = (clean_first())
    -- ok: lua_call_results
    sink(a)
    -- ok: lua_call_results
    sink(b)
end

function parenthesised_call_first_tainted()
    local a, b = (two())
    -- ruleid: lua_call_results
    sink(a)
end

function two_results_second_tainted()
    local a, b = clean_first()
    -- ruleid: lua_call_results
    sink(b)
end
