# Never imported by main.py.  Its [run] takes exactly the one argument the call
# passes, so arity-only resolution picks it and reports a flow that cannot
# happen.
class Widget:
    def run(self, data):
        # ok: homonym-class-wrong-file
        sink(data)
