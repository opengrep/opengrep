class Holder:
    def __init__(self):
        self.items = []
        self.values = []
        self.pop = []


def items_field():
    a = Holder()
    a.items.append(source())
    # ruleid: field-named-like-zero-argument-method
    sink(a.items)


def values_field():
    a = Holder()
    a.values.append(source())
    # ruleid: field-named-like-zero-argument-method
    sink(a.values)


def pop_field():
    a = Holder()
    a.pop.append(source())
    # ruleid: field-named-like-zero-argument-method
    sink(a.pop)


def items_field_clean():
    a = Holder()
    a.items.append("safe")
    # ok: field-named-like-zero-argument-method
    sink(a.items)


class Attributes:
    def __init__(self):
        self.items = None
        self.other = None


def attribute_under_items():
    a = Attributes()
    a.items.k = source()
    # ruleid: field-named-like-zero-argument-method
    sink(a.items)


def attribute_under_other():
    a = Attributes()
    a.other.k = source()
    # ruleid: field-named-like-zero-argument-method
    sink(a.other)
