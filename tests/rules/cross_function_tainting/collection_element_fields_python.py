class R:
    def __init__(self, path, other):
        self.path = path
        self.other = other


def iterated():
    infos = []
    infos.append(R(source(), "x"))
    for i in infos:
        # ruleid: collection_element_fields_python
        sink(i.path)
        # ok: collection_element_fields_python
        sink(i.other)


def element_read():
    infos = []
    infos.append(R(source(), "x"))
    e = infos.pop()
    # ruleid: collection_element_fields_python
    sink(e.path)
    # ok: collection_element_fields_python
    sink(e.other)


def indexed():
    infos = []
    infos.append(R(source(), "x"))
    # ruleid: collection_element_fields_python
    sink(infos[0].path)
    # ok: collection_element_fields_python
    sink(infos[0].other)


def show_path(i):
    # ruleid: collection_element_fields_python
    sink(i.path)


def show_other(i):
    # ok: collection_element_fields_python
    sink(i.other)


def callback():
    infos = []
    infos.append(R(source(), "x"))
    list(map(show_path, infos))
    list(map(show_other, infos))


def copied():
    infos = []
    infos.append(R(source(), "x"))
    for i in infos.copy():
        # ruleid: collection_element_fields_python
        sink(i.path)
        # ok: collection_element_fields_python
        sink(i.other)
