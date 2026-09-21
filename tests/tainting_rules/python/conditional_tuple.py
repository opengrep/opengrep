def whole_then_tuple(self):
    path, query = self.path.split("?", 1) if "?" in self.path else (self.path, "")
    # ruleid: conditional_tuple
    sink(path)
    # ruleid: conditional_tuple
    sink(query)


def tuple_then_whole(self):
    path, query = (self.path, "") if "?" not in self.path else self.path.split("?", 1)
    # ruleid: conditional_tuple
    sink(path)
    # ruleid: conditional_tuple
    sink(query)


def two_tuples(self, c):
    a, b = (self.path, "") if c else ("", "x")
    # ruleid: conditional_tuple
    sink(a)
    # ok: conditional_tuple
    sink(b)


def tuple_or_clean_whole(self, c):
    a, b = (self.path, "") if c else other()
    # ruleid: conditional_tuple
    sink(a)
    # ok: conditional_tuple
    sink(b)


def as_statement(self):
    if "?" in self.path:
        pair = self.path.split("?", 1)
    else:
        pair = (self.path, "")
    # ruleid: conditional_tuple
    sink(pair[1])
