from source import source

def inner_only_dead():
    if False:
        return source()
    return ""

