from pkg_b.config import Config


def go():
    Config().load().run(source())
