# Never imported by main.py.  Same class name, same method name, same arity as
# worker_a.Widget.run — the collision must not cost worker_a its resolution.
class Widget:
    def run(self, data):
        # ok: homonym-class-collision
        sink(data)
