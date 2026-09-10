class A:
    def run(self):
        pass


class B:
    def run(self):
        pass


def go(a: A, b: B):
    # ruleid: method_unification_survives_taint
    a.run()
    b.run()
    x = source()
    # ruleid: method_unification_survives_taint_sink
    sink(x)
