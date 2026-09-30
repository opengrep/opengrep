class Base:
    def handle(self, data):
        # ruleid: member-after-unresolved-base-grandchild
        sink(data)


class ShadowedBase:
    def handle(self, data):
        # ok: member-after-unresolved-base-grandchild
        sink(data)
