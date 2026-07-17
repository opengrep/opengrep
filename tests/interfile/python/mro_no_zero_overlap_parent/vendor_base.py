class Base:
    def process(self, data):
        # Unrelated module's Base: app.Worker(Base) must NOT inherit
        # this method — its Base comes from an unresolvable import, and
        # this qn shares no prefix with app's.
        # ok: mro-no-zero-overlap-parent
        sink(data)
