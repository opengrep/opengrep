from apply import apply
from handler import handler

def wrapper(flag, x):
    return apply(handler, flag, x)


