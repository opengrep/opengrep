# A module of the same name elsewhere in the project, never imported by
# sub/app.py: same class, same method, same arity.
class Store:
    def save(self, data):
        # ok: same-name-module-other-dir
        sink(data)
