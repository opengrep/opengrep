def later_mapping_case_is_reachable(m):
    x = taint_source()
    match m:
        case {"a": a}:
            g(a)
        case {"b": _}:
            # ruleid: taint-switch-case-python
            sink(x)
