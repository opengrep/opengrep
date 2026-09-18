from base import Base


class Svc(Base):
    @classmethod
    def start(cls, data):
        cls.helper(data)
