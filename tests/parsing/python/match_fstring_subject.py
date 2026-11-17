# The subject of a match statement can contain f-strings, with a format
# specification that holds a colon.
match f"{x:>3}":
    case "a":
        pass

match (f"{x}", y):
    case ("a", _):
        pass
