from vendor_package import Mixin
from mixins import LocalMixin
from base import Base, ResolvedBase, ShadowedBase


class Child(Mixin, Base):
    pass


class ResolvedChild(LocalMixin, ResolvedBase):
    pass


class OwnMember(Mixin, ShadowedBase):
    def handle(self, data):
        return data
