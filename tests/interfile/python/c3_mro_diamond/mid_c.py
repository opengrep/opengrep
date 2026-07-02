from base_a import A

class C(A):
    def m(self, q):
        # ruleid: c3-mro-override
        sink(q)
