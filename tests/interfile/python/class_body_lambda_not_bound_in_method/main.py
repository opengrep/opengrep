from util import escape


class V:
    escape = lambda x: x

    def m(self, x):
        return escape(x)


V().m(source())
