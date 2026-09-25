def direct(x, t):
    x = other()
    if x != 7:
        return
    # ruleid: test-lambda-stale-guard-python
    sink(t)

def via_lambda(x, t):
    x = other()
    if x != 7:
        return
    # ruleid: test-lambda-stale-guard-python
    (lambda: sink(t))()

def main():
    direct(1, source())
    via_lambda(1, source())
