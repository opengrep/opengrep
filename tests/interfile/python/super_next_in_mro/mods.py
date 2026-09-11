class Skip:
    pass


class Base:
    def run(self, data):
        # ruleid: super-next-in-mro
        sink(data)


class Child(Skip, Base):
    def run(self, data):
        super().run(data)
