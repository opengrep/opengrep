g = source()
safe = "ok"

# ruleid: test-module-global-source-python
sink(g)


def main():
    # ruleid: test-module-global-source-python
    sink(g)


def control():
    # ok: test-module-global-source-python
    sink(safe)
