from vendor_package import Mixin
from base import Base, ShadowedBase


class Mid(Mixin, Base):
    pass


class OwnMid(Mixin, ShadowedBase):
    def handle(self, data):
        return data
