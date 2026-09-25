from base import Base


class Sub(Base):
    def m(self, x):
        # ruleid: self_call_reaches_override_in_other_file
        sink(x)


def main():
    Sub().run(source())
