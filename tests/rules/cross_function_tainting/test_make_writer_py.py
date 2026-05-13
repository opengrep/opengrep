# Minimal f-returns-g exercise: make_writer takes a target and returns
# a lambda that mutates it via list.append (modelled as
# ArgTaintsThis -> BThis mutation). The lambda's signature carries a
# ToLval against its captured target (a BArg ref to make_writer's
# parameter); at make_writer's call site substitute_in_sig refines
# that ref to the caller's actual list, so when the returned lambda
# is invoked the write lands on out.

def make_writer(target):
    def writer(v):
        target[0] = v
    return writer

def driver():
    out = []
    w = make_writer(out)
    w(source())
    # ruleid: test-make-writer-py
    sink(out)
