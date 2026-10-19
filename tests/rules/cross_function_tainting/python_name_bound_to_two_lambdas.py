# A name bound to two lambdas holds either of them at the call.
if cond:
    h = lambda x: "safe"
else:
    h = lambda x: x
# ruleid: python_name_bound_to_two_lambdas
sink(h(source()))
