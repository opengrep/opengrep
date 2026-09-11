from zzz import Foo


def source():
    return "tainted"


def main():
    tainted = source()
    # Foo here is zzz.Foo (extends Base).  Foo() triggers super().__init__
    # which routes through Type_state.parent_class to Base.__init__,
    # storing tainted into self.value.  The subsequent .process() call
    # resolves via inheritance to Base.process, where the sink lives.
    f = Foo(tainted)
    f.process()


main()
