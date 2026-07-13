# Every class attribute initialized to [] and later appended to should be
# matched, not just the first one.
class C:
    # ruleid: class_field_multi_match
    a = []

    def use_a(self, p):
        a.append(p)

    # ruleid: class_field_multi_match
    b = []

    def use_b(self, p):
        b.append(p)

    # ruleid: class_field_multi_match
    c = []

    def use_c(self, p):
        c.append(p)
