from pkg_a.config import Config


def go():
    Config().load().run(source())
