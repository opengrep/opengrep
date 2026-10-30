# In a method, a bare name refers to the module and builtins, not to the
# methods of the class: 'load(...)' is the function below, 'self.load(...)'
# is the method.

def load(path):
    return "constant"


class Cache:
    def load(self, path):
        return path

    def bare(self):
        # ok: test-bare-call-in-method
        sink(load(source()))

    def through_self(self):
        # ruleid: test-bare-call-in-method
        sink(self.load(source()))
