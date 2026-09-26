# A tuple or dict built from the parameters and returned keeps its parts
# apart: the part that holds a clean argument carries no taint at the caller.


def one(uid, folder):
    return (uid, folder)


def as_dict(uid, folder):
    return {"uid": uid, "folder": folder}


def indexed():
    t = one(source(), "x")
    # ruleid: composite_return_field_python
    sink(t[0])
    # ok: composite_return_field_python
    sink(t[1])


def unpacked():
    a, b = one(source(), "x")
    # ruleid: composite_return_field_python
    sink(a)
    # ok: composite_return_field_python
    sink(b)


def keyed():
    d = as_dict(source(), "x")
    # ruleid: composite_return_field_python
    sink(d["uid"])
    # ok: composite_return_field_python
    sink(d["folder"])


def local():
    t = (source(), "x")
    # ruleid: composite_return_field_python
    sink(t[0])
    # ok: composite_return_field_python
    sink(t[1])
