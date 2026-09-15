# Never imported by main.py.  Same class name, same method name, same arity as
# pkg.Widget.run — the collision must not cost the package class its
# resolution.
class Widget:
    def run(self, data):
        # ok: same-name-class-package-init
        sink(data)
