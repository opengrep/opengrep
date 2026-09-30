class Base:
    def handle(self, data):
        # ruleid: member-after-unresolved-base
        sink(data)


class ResolvedBase:
    def handle(self, data):
        # ruleid: member-after-unresolved-base
        sink(data)


class ShadowedBase:
    def handle(self, data):
        # ok: member-after-unresolved-base
        sink(data)
