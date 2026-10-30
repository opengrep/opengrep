def tainted():
    path = source()

    def use():
        # ruleid: python_nested_def_reads_enclosing_local
        sink(path)

    use()


def clean():
    path = "fixed"

    def use():
        # ok: python_nested_def_reads_enclosing_local
        sink(path)

    use()
