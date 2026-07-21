# Wildcard import from the package: brings in exactly lib.__all__ (_run),
# not the public-but-unlisted helper.
from lib import *


def source():
    return input()


def main():
    _run(source())
    helper(source())
