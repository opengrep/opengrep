from vendor_package import Mixin


class Base:
    def handle(self, data):
        # ruleid: member_after_unresolved_base_python
        sink(data)


class ShadowedBase:
    def handle(self, data):
        # ok: member_after_unresolved_base_python
        sink(data)


class Child(Mixin, Base):
    pass


class OwnMember(Mixin, ShadowedBase):
    def handle(self, data):
        return data


def run():
    Child().handle(source())


def run_own():
    OwnMember().handle(source())
