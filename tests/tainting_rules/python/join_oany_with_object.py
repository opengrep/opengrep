def oany_then_list(self, c, k):
    if c:
        x = self.path
        x[k] = self.path
    else:
        x = [self.path, ""]
    # ruleid: join_oany_with_object
    sink(x[0])
    # ruleid: join_oany_with_object
    sink(x[1])


def list_then_oany(self, c, k):
    if c:
        x = [self.path, ""]
    else:
        x = self.path
        x[k] = self.path
    # ruleid: join_oany_with_object
    sink(x[1])


def oany_control(self, k):
    x = self.path
    x[k] = self.path
    # ruleid: join_oany_with_object
    sink(x[1])


def clean_oany_then_list(self, c, k):
    if c:
        x = other()
        x[k] = ""
    else:
        x = [self.path, ""]
    # ruleid: join_oany_with_object
    sink(x[0])
    # ok: join_oany_with_object
    sink(x[1])
