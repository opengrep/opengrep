checker = 0


def checker(a):
    return a


checker(1)

handler = 0
handler(2)


def handler(a):
    return a


counter = 0


def bump():
    global counter
    counter = counter + 1
    return counter
