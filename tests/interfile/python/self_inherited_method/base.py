class Base:
    # Inherited @staticmethod, called as self.opt(...) despite no self param.
    @staticmethod
    def opt(name):
        # ruleid: self-inherited-method
        sink(name)

    # Inherited instance method, called as self.helper(...).
    def helper(self, x):
        # ruleid: self-inherited-method
        sink(x)
