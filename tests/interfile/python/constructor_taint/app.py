import os
from store import Store

def source():
    return os.environ.get("SECRET")

def main():
    tainted = source()
    s = Store(tainted)
    s.send()

main()
