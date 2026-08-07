from store import Store


class Service:
    def handle(self):
        # Constructor call written INSIDE a method (no `new` keyword in
        # Python). Must resolve to Store.__init__ cross-file.
        tainted = source()
        Store(tainted)

    def url_for(self, cls):
        # A class passed as an ARGUMENT is not a construction: url_for
        # must not gain a dispatch edge to Store.__init__.
        return str(cls)

    def use(self):
        # ok: ctor-call-in-method
        self.url_for(Store)
