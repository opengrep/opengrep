from lib import Util, Fire, Untyped, Smoke

def go():
    Util.emit(1, Fire())

def go_untyped():
    Untyped.emit_untyped(1, Smoke())
