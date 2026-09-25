# An assignment to an imported name rebinds it: a later call calls the new
# value, here an integer, and not the imported function.
from lib import checker, handler

handler = 0
handler(source())


def run():
    checker(source())
