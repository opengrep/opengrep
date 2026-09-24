def whole_then_tuple(self):
    path, query = self.path.split("?", 1) if "?" in self.path else (self.path, "")
    # ruleid: join_whole_with_object
    sink(path)
    # ruleid: join_whole_with_object
    sink(query)


def tuple_then_whole(self):
    path, query = (self.path, "") if "?" not in self.path else self.path.split("?", 1)
    # ruleid: join_whole_with_object
    sink(path)
    # ruleid: join_whole_with_object
    sink(query)


def two_tuples(self, c):
    a, b = (self.path, "") if c else ("", "x")
    # ruleid: join_whole_with_object
    sink(a)
    # ok: join_whole_with_object
    sink(b)


def tuple_or_clean_whole(self, c):
    a, b = (self.path, "") if c else other()
    # ruleid: join_whole_with_object
    sink(a)
    # ok: join_whole_with_object
    sink(b)


def as_statement(self):
    if "?" in self.path:
        pair = self.path.split("?", 1)
    else:
        pair = (self.path, "")
    # ruleid: join_whole_with_object
    sink(pair[1])


def nested(self, c):
    x = self.path if c else ((self.path, ""), "")
    # ruleid: join_whole_with_object
    sink(x[0][1])


def dict_literal(self, c):
    x = self.path if c else {"a": self.path, "b": ""}
    # ruleid: join_whole_with_object
    sink(x["b"])


def list_literal(self, c):
    x = self.path if c else [self.path, ""]
    # ruleid: join_whole_with_object
    sink(x[1])


def whole_with_a_field_of_its_own(self, c):
    if c:
        x = self.path
        x.extra = self.path
    else:
        x = {"a": self.path, "b": ""}
    # ruleid: join_whole_with_object
    sink(x["b"])


def sanitized_on_one_path(self, c):
    if c:
        x = self.path
    else:
        x = {"a": self.path, "b": self.path}
        x["b"] = sanitize(x["b"])
    # ruleid: join_whole_with_object
    sink(x["b"])


def sanitized_on_both_paths(self, c):
    if c:
        x = {"a": self.path, "b": self.path}
        x["b"] = sanitize(x["b"])
    else:
        x = {"a": "", "b": self.path}
        x["b"] = sanitize(x["b"])
    # ok: join_whole_with_object
    sink(x["b"])
