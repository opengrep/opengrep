from unindexed_thirdparty import *


class Worker(Base):
    # Base comes from the star import (unresolvable: the module is not
    # in the project).  It must stay unresolved rather than bind to
    # vendor_base.Base.
    pass


def run():
    w = Worker()
    w.process(taint())
