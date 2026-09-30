from child import Child, ResolvedChild, OwnMember


def run():
    Child().handle(source())


def run_resolved():
    ResolvedChild().handle(source())


def run_own():
    OwnMember().handle(source())
