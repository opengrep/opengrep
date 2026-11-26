class Model:
    def save(self, x):
        # ok: test-python-named-import-after-star-import
        sink(x)


class Record:
    def save(self, x):
        # ruleid: test-python-named-import-after-star-import
        sink(x)
